{-# LANGUAGE OverloadedStrings,GADTs,DeriveDataTypeable,DeriveFunctor,GeneralizedNewtypeDeriving,MultiParamTypeClasses,QuasiQuotes,TemplateHaskell,TypeFamilies,PackageImports,NamedFieldPuns,RecordWildCards,TypeSynonymInstances,FlexibleContexts #-}
module Main where

import           Data.Crawler
import           Data.CrawlerParameters
import           Misc
import           WWW.SimpleTable

import           Control.Applicative
import           Control.Concurrent
import qualified Control.Exception as Ex
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Error
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Writer.Lazy
import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as AP
import           Data.ByteString (ByteString,empty,writeFile)
import qualified Data.ByteString as B (empty,writeFile)
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import           Data.Time
import           Data.Word
import           Database.Persist
import qualified Database.Persist.MongoDB as Mongo
import           Database.Persist.TH
import           Language.Haskell.TH.Syntax (Type(ConT))
import           Network (PortID (PortNumber))
import           Network.Curl
import           Network.Socket (PortNumber(..))
import "network" Network.URI
import           System.Console.CmdArgs.Implicit
import           System.Exit (ExitCode(..),exitWith)
import           System.FilePath.Posix
import           Text.HTML.TagSoup
import qualified Text.Pandoc as P
import           Text.Poppler

main :: IO ()
main = do
  (mongoP,crawlP) <- processParams
  docs <- crawlBisBcbsPublications (courtesyPeriod crawlP)
          (publishedFrom crawlP) (publishedUntil crawlP)
  Ex.catch
    (do
      Mongo.withMongoDBConn (db mongoP) (host mongoP) (port mongoP) (auth mongoP) (dt mongoP) $ \pool -> do
        Mongo.runMongoDBPool Mongo.master (writeDocs (courtesyPeriod crawlP)
                                           docs (pdfPath crawlP)) pool)
    gobalExceptionHandler

gobalExceptionHandler :: Ex.SomeException -> IO ()
gobalExceptionHandler e = do
  putStrLn $ "gobalExceptionHandler: Got an exception --> " ++ show e

writeDocs :: Int -> [BisDoc] -> FilePath -> Mongo.Action IO ()
writeDocs ct docs pdfPath = do
      liftIO $ putStrLn "Insert publications..."
      forM_ docs (\d -> writeOneDoc d ct pdfPath)

writeDocsHandler :: Ex.SomeException -> IO (Either (Int, String) Text)
writeDocsHandler e = do
  putStrLn $ "writeDocsHandler: Got an exception --> " ++ show e
  return $ Left (-1,show e)

writeOneDoc :: BisDoc -> Int -> FilePath -> Mongo.Action IO ()
writeOneDoc d ct pdfPath = do
          let title = T.unpack $ bisDocTitle d
          liftIO $ putStrLn $ "Download and process '"++title++"'"
          case bisDocDetails d of
              Just det -> case bisDocumentDetailsFullTextLink det of
                Just url -> do
                  mFullPdf <- liftIO $ getFile ct url
                  case mFullPdf of
                    Just fullPdf -> do
                      fullHtml <- liftIO $ Ex.handle writeDocsHandler $ do
                        let fname = snd (splitFileName (uriPath (fromJust (parseURI url))))
                        B.writeFile (pdfPath++fname) fullPdf
                        fullHtml <- pdfToHtmlTextOnly [] fullPdf
                        return fullHtml
                      case fullHtml of
                        Right html -> do
                          let fullMd = htmlToMarkdown html
                          let det' = det{bisDocumentDetailsFullTextMarkdown=Just fullMd}
                          Mongo.insert_ (d {bisDocDetails=Just det'})
                        Left (e,stderr) -> do
                          liftIO $ putStrLn $ "Error: "++show e
                          liftIO $ putStrLn stderr
                          Mongo.insert_ d
                    Nothing -> do
                      liftIO $ putStrLn $ "Error: Could not load pdf file."
                      Mongo.insert_ d
                Nothing -> do
                  liftIO $ putStrLn $ "No file to download for '"++title++"'"
                  Mongo.insert_ d
              Nothing -> do
                liftIO $ putStrLn $ "No details for '"++title++"'"
                Mongo.insert_ d

type TTag = Tag Text

getFile :: Int -> URLString -> IO (Maybe ByteString)
getFile ct url = Ex.catch
              (do
                  resp <- curlGetResponse_ url [] :: IO (CurlResponse_ [(String,String)] ByteString)
                  threadDelay ct
                  return $ Just $ respBody resp)
              handler
  where handler :: Ex.SomeException -> IO (Maybe ByteString)
        handler e = do
          putStrLn $ "getFile: Got an exception --> " ++ show e
          return Nothing

openUrlUtf8 :: Int -> URLString -> IO Text
openUrlUtf8 ct url =
  Ex.catch (do
               resp <- curlGetResponse_ url []
                       :: IO (CurlResponse_ [(String,String)] ByteString)
               threadDelay ct
               return (T.decodeUtf8 (respBody resp)))
            handler
  where handler :: Ex.SomeException -> IO Text
        handler e = do
          putStrLn $ "openUrlUtf8: Got an exception --> " ++ show e
          return ""

collectWhile :: Monad m => Maybe t -> (t -> m [a]) -> (t -> m (Maybe t)) -> [a] -> m [a]
collectWhile (Just jx) process next bag = do
  x1 <- next jx
  new <- process jx
  collectWhile x1 process next (bag++new)
collectWhile Nothing _ _ bag = return bag

ppTags tags = putStrLns (map showT tags)

bisSite :: URLString
bisSite = "http://www.bis.org"

crawlBisBcbsPublications :: Int -> Maybe Day -> Maybe Day -> IO [BisDoc]
crawlBisBcbsPublications ct t0 t1 = withCurlDo $ do
  putStrLn $ "Start with page "++startingPoint
  src <- openUrlUtf8 ct startingPoint
  if src==""
    then do
      T.putStrLn "Cannot get page."
      return []
    else do
      let tags = parseTags src
      collectWhile (Just tags) (processBISDocPage ct t0 t1) (getNextBISDocPage ct) []
  where startingPoint = bisSite++"/bcbs/publications.htm"

processBISDocPage :: Int -> Maybe Day -> Maybe Day -> [TTag] -> IO [BisDoc]
processBISDocPage ct t0 t1 tags = do
  let ts = simpleTables tags
  if null ts
     then do
       T.putStrLn "No table found on page."
       return []
    else do
       let ts1 = head ts
       docs <- catMaybes <$> mapM (processDoc ct t0 t1) (rows ts1)
       T.putStrLn "Found: "
       T.putStrLn $ T.intercalate "\n" (map (\ d -> "-- " `T.append` (bisDocTitle d)) docs)
       return docs

processDoc :: Int -> Maybe Day -> Maybe Day -> TableRow -> IO (Maybe BisDoc)
processDoc ct t0 t1 row = do
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
            then do
              resp <- curlGetResponse_ l [] :: IO (CurlResponse_ [(String,String)] ByteString)
              threadDelay ct
              return $ Just BisDocumentDetails {
                bisDocumentDetailsDocumentTitle=ttl,
                bisDocumentDetailsSummary = "",
                bisDocumentDetailsFullTextLink = Just l,
                bisDocumentDetailsFullTextMarkdown = Nothing,
                bisDocumentDetailsLocalFile = Just $ filenameFromUrl l,
                bisDocumentDetailsOtherLinks = []}
            else analyzeBisSingleDocPage ct l
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

getNextBISDocPage :: Int -> [TTag] -> IO (Maybe [TTag])
getNextBISDocPage ct tags = do
  let nextLink = getNextDocLink tags
  case nextLink of
    Just lnk -> do
      putStrLn $ "Follow link: "++lnk
      src <- openUrlUtf8 ct lnk
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

analyzeBisSingleDocPage :: Int -> URLString -> IO (Maybe BisDocumentDetails)
analyzeBisSingleDocPage ct url = do
  putStrLn $ "Analyse "++url
  allTags <- return . partitionIt =<< (return . parseTags) =<< openUrlUtf8 ct url
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
                            in T.pack $
                               P.writeMarkdown P.def $
                               P.readHtml P.def $
                               T.unpack $
                               renderTags summaryHtml
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
           putStrLn $ "Cannot parse page because structure has changed. "
             ++"Content:\n"
             ++show allTags
           return Nothing
    else do
      putStrLn "No input. Probably You are not connected to the internet."
      return Nothing
    
  where tagContent = TagOpen "h1" [] :: TTag
        tagAnnotations = TagOpen "div" [("id","right"),
                                        ("class","column"),
                                        ("role","complementary")] :: TTag
        tagFooter = TagOpen "div" [("id","footer"),("role","contentinfo")] :: TTag
        partitionIt = partitions (\tag -> tag~==tagContent
                                          || tag~==tagAnnotations
                                          || tag~==tagFooter)
        divFullText = TagOpen "div" [("class","list_box full_text")] :: TTag
        otherBox = TagOpen "div" [("class","list_box")] :: TTag

processOtherBox :: [Tag Text] -> DocumentLink
processOtherBox tags = let t1 = sections (~== (TagOpen "h4" [] :: TTag)) tags
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
                       in (DocumentLink typ links)
