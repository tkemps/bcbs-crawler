{-# LANGUAGE OverloadedStrings,NamedFieldPuns,RecordWildCards #-}
module Main where

import           Control.Monad.Crawler
import           Data.Crawler
import           Data.CrawlerParameters
import           Misc
import           Text.Poppler
import           Text.HTML.SimpleTable

import           Control.Applicative
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import qualified Data.Attoparsec.Text as AP (parseOnly)
import qualified Data.ByteString as B (writeFile)
import           Data.List
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Database.Persist.MongoDB as Mongo
import           Text.HTML.TagSoup
import qualified Text.Pandoc as P

main :: IO ()
main = do
  (mongoP,crawlP) <- processParams
  putStrLn $ "This is "++_PROGRAM_INFO
  (e,msgs) <- runCrawler mongoP crawlP $
              (do
                  docs <- crawlBisBcbsPublications
                  liftIO $ putStrLn "Insert publications..."
                  forM_ docs writeOneDoc)
              `catch` (\e -> liftIO $ do
                          putStrLn $ "bcbs-crawl: "++show (e :: SomeException)
                          putStrLn $ "Cannot continue processing.")
  print e
  print msgs

type TTag = Tag Text

writeOneDoc :: BisDoc -> Crawler ()
writeOneDoc d = do
  let title = T.unpack $ bisDocTitle d
  liftIO $ putStrLn $ "Download and process '"++title++"'"
  case bisDocDetails d of
    Just det -> case bisDocumentDetailsFullTextLink det of
      Just url -> do
        fullPdf <- getFile' url
        p <- pdfPath <$> ask
        liftIO $ B.writeFile (p++(filenameFromUrl url)) fullPdf
        fullHtml <- liftIO $ pdfToHtmlTextOnly [] fullPdf
        case fullHtml of
          Right html -> do
            let fullMd = htmlToMarkdown html
            let det' = det{bisDocumentDetailsFullTextMarkdown=Just fullMd}
            liftMongo $ Mongo.insert_ (d {bisDocDetails=Just det'})
          Left (e,stderr) -> do
            liftIO $ putStrLn $ "Error: "++show e
            liftIO $ putStrLn stderr
            liftMongo $ Mongo.insert_ d
      Nothing -> do
        liftIO $ putStrLn $ "No file to download for '"++title++"'"
        liftMongo $ Mongo.insert_ d
    Nothing -> do
      liftIO $ putStrLn $ "No details for '"++title++"'"
      liftMongo $ Mongo.insert_ d

bisSite = "http://www.bis.org"
startingPoint = bisSite++"/bcbs/publications.htm"

crawlBisBcbsPublications :: Crawler [BisDoc]
crawlBisBcbsPublications = do
  liftIO $ putStrLn $ "Start with page "++startingPoint
  src <- openUrlUtf8' startingPoint
  if src==""
    then do
      liftIO $ putStrLn "Cannot get page."
      return []
    else do
      let tags = parseTags src
      collectWhile (Just tags) processBISDocPage getNextBISDocPage []

collectWhile :: Monad m => Maybe t -> (t -> m [a]) -> (t -> m (Maybe t)) -> [a] -> m [a]
collectWhile (Just jx) process next bag = do
  x1 <- next jx
  new <- process jx
  collectWhile x1 process next (bag++new)
collectWhile Nothing _ _ bag = return bag

processBISDocPage :: [TTag] -> Crawler [BisDoc]
processBISDocPage tags = do
  let ts = simpleTables tags
  if null ts
     then do
       liftIO $ putStrLn "No table found on page."
       return []
    else do
       let ts1 = head ts
       docs <- catMaybes <$> mapM processDoc (rows ts1)
       liftIO $ putStrLn "Found: "
       liftIO $ T.putStrLn $
         T.intercalate "\n" (map (\ d -> "-- " `T.append` (bisDocTitle d)) docs)
       return docs

getNextBISDocPage :: [TTag] -> Crawler (Maybe [TTag])
getNextBISDocPage tags = do
  let nextLink = getNextDocLink tags
  case nextLink of
    Just lnk -> do
      liftIO $ putStrLn $ "Follow link: "++lnk
      src <- openUrlUtf8' lnk
      let tags1 = parseTags src
      return (Just tags1)
    Nothing -> return Nothing

getNextDocLink :: [TTag] -> Maybe URLString
getNextDocLink tags =
  let s = sections (~== (TagOpen ("a"::Text) [("class","next")])) tags
  in if null s
     then Nothing             
     else let t = head (head s)
          in let href = fromAttrib "href" t
             in if href==""
                then Nothing
                else Just (if T.head href=='/'
                           then bisSite++(T.unpack href)
                           else T.unpack href)

processDoc :: TableRow -> Crawler (Maybe BisDoc)
processDoc row = do
  ct <- courtesyPeriod <$> ask
  t0 <- publishedFrom <$> ask
  t1 <- publishedUntil <$> ask
  let es = elements row
      dt = getDateFromRow es
      typ = getTypeFromRow es
      lnk = getLinkFromRow es
      ttl = getTitleFromRow es
  if not (between dt t0 t1)
    then return Nothing
    else do
      details <- case lnk of
        Nothing -> return Nothing
        Just l ->
          if "pdf" `isSuffixOf` l
          then return $ Just BisDocumentDetails {
            bisDocumentDetailsDocumentTitle=ttl,
            bisDocumentDetailsSummary = "",
            bisDocumentDetailsFullTextLink = Just l,
            bisDocumentDetailsFullTextMarkdown = Nothing,
            bisDocumentDetailsLocalFile = Just $ filenameFromUrl l,
            bisDocumentDetailsOtherLinks = []}
          else analyzeBisSingleDocPage l
      return $ Just $ BisDoc dt typ lnk ttl details

getDateFromRow tags = let ds = deleteAll ["\n","\t","\r"] $ fromTagText (head (head tags))
                      in case AP.parseOnly parseDate ds of
                        Left m -> error $ "Parse error while processing a date: "++m
                        Right d -> d

getTypeFromRow tags = let tag = (tags!!1)!!3
                      in if isTagOpen tag
                            then let attr = fromAttrib "title" tag
                                 in if attr=="" then Nothing else Just attr
                         else Nothing

getLinkFromRow :: [[TTag]] -> Maybe URLString
getLinkFromRow tags = let tag = (tags!!2)!!3
                      in if isTagOpen tag
                         then Just $ bisSite++(T.unpack $ fromAttrib "href" tag)
                         else Nothing

getTitleFromRow tags = let tag = (tags!!2)!!4
                       in if isTagText tag
                          then deleteAll ["\t","\r","\n"] $ fromTagText tag
                          else ""

analyzeBisSingleDocPage :: URLString -> Crawler (Maybe BisDocumentDetails)
analyzeBisSingleDocPage url = do
  liftIO $ putStrLn $ "Analyse "++url
  allTags <- return . partitionIt =<< (return . parseTags) =<< openUrlUtf8' url
  if not (null allTags)
    then if length allTags==3
         then do
           let contentTags = allTags!!0
               annotationTags = allTags!!1
               docTitle = fromTagText (contentTags!!1)
               docSummary = let divSections = sections (~== (TagClose ("div"::Text))) contentTags
                                summaryHtml = if length divSections>=2
                                              then purgeHtmlText $ tail (divSections!!1)
                                              else error "Cannot parse page because structure has changed."
                            in htmlToMarkdown (renderTags summaryHtml)
               fullTextLink = let fBox = sections (~==divFullText) annotationTags
                              in if not (null fBox)
                                 then let aTag = sections (~==aLinkTag) (head fBox)
                                          link = T.unpack $ fromAttrib "href" (head $ head aTag)
                                      in if "/" `isPrefixOf` link
                                         then Just $ bisSite++link
                                         else Just link
                                 else Nothing
           let otherBoxes = partitions (~== otherBox) annotationTags
           let others = map processOtherBox otherBoxes
           return $ Just $ BisDocumentDetails docTitle docSummary fullTextLink Nothing
             (fmap filenameFromUrl fullTextLink) others
         else do
           liftIO $ putStrLn $ "Cannot parse page because structure has changed. "
             ++"Content:\n"
             ++show allTags
           return Nothing
    else do
      liftIO $ putStrLn "No input. Probably You are not connected to the internet."
      return Nothing

tagContent = TagOpen "h1" [] :: TTag

tagAnnotations = TagOpen "div" [("id","right"),
                                ("class","column"),
                                ("role","complementary")] :: TTag

tagFooter = TagOpen "div" [("id","footer"),("role","contentinfo")] :: TTag

partitionIt = partitions (\tag -> tag~==tagContent
                                  || tag~==tagAnnotations
                                  || tag~==tagFooter)

divFullText = TagOpen "div" [("class","list_box full_text")] :: TTag

otherBox = TagOpen "div" [("class","list_box")] :: TTag

processOtherBox :: [TTag] -> DocumentLink
processOtherBox tags =
  let t1 = sections (~== (TagOpen "h4" [] :: TTag)) tags
      typ = fromTagText ((head t1)!!1)
      t2 = partitions (~==aLinkTag) tags
      link tags = if null tags
                  then ""
                  else let lnk' = T.unpack $ fromAttrib "href" (head tags)
                           lnk = if lnk'==""
                                 then ""
                                 else if "/" `isPrefixOf` lnk'
                                      then bisSite++lnk'
                                      else lnk'
                       in lnk
      links = map link t2
  in DocumentLink typ links
