{-# LANGUAGE OverloadedStrings #-}
module BcbsCrawler where

import Control.Applicative
import qualified Control.Exception as Ex
import Control.Monad
import Data.Char
import Data.List
import qualified Data.Attoparsec.Text as AP
import Data.Attoparsec.Text (Parser)
import qualified Data.ByteString.Lazy as B (writeFile)
import Data.ByteString (ByteString)
import Data.Csv (encode,ToRecord(..),record,toField)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import Data.Text (Text)
import Data.Time
import Network.Curl
import Text.HTML.TagSoup
import qualified Text.Pandoc as P

type TTag = Tag Text

data BISDoc = BISDoc {
  date :: Day,
  typ :: Maybe BISDocType,
  link :: Maybe Text,
  title :: Text
  } deriving (Show)

data BISDocType = Guidelines | Implementation | Others | SoundPractices | Standards
                deriving (Eq,Show)

textToBISDocType t =
  if t=="Guidelines" then Guidelines
  else if t=="Implementation" then Implementation
       else if t=="Others" then Others
            else if t=="Sound practices" then SoundPractices
                 else if t=="Standards" then Standards
                      else error $ T.unpack $ T.concat ["I don't know the document type ",t]

instance ToRecord BISDoc where
     toRecord (BISDoc date typ ttl lnk) =
       record [toField (show date), toField (showMaybe typ), toField ttl, toField lnk]

showMaybe (Just x) = show x
showMaybe Nothing = ""

writeDocs :: [BISDoc] -> IO ()
writeDocs docs = B.writeFile "bis-bcbs-docs.txt" (encode docs)

putStrLns :: [Text] -> IO ()
putStrLns = T.putStrLn . T.concat

showT :: (Show s) => s -> Text
showT = T.pack . show

openUrlUtf8 :: Text -> IO Text
openUrlUtf8 url = do
  resp <- curlGetResponse_ (T.unpack url) [] :: IO (CurlResponse_ [(String,String)] ByteString)
  return (T.decodeUtf8 (respBody resp))

collectWhile :: Monad m => Maybe t -> (t -> m [a]) -> (t -> m (Maybe t)) -> [a] -> m [a]
collectWhile (Just jx) process next bag = do
  x1 <- next jx
  new <- process jx
  collectWhile x1 process next (bag++new)
collectWhile Nothing _ _ bag = return bag

ppTags tags = putStrLns (map showT tags)

bisSite :: Text
bisSite = "http://www.bis.org"

crawlBisBcbsPublications :: IO [BISDoc]
crawlBisBcbsPublications = withCurlDo $ do
  putStrLns ["Start with page ",startingPoint]
  src <- openUrlUtf8 startingPoint
  if src==""
    then do
      T.putStrLn "Cannot get page."
      return []
    else do
      let tags = parseTags src
      collectWhile (Just tags) processBISDocPage getNextBISDocPage []
  where startingPoint = T.concat [bisSite,"/bcbs/publications.htm"]

processBISDocPage :: [TTag] -> IO [BISDoc]
processBISDocPage tags = do
  let ts = simpleTables tags
--  putStrLns ["No. of tables: ",showT (length ts)]
  if null ts
     then do
       T.putStrLn "No table found on page."
       return []
    else do
       let t1 = head ts
--       putStrLns ["Head line in first table: ",showT (header t1)]
--       putStrLns ["No. of rows in first table: ",showT (length (rows t1))]
--       putStrLns ["No. of columns in first row: ",showT (length $ elements $ (rows t1)!!1)]
--       putStrLns ["First row in first table: ",showT (elements $ (rows t1)!!0)]
       let docs = map (\r -> let es = elements r
                             in BISDoc (getDateFromRow es)
                                (getTypeFromRow es)
                                (getLinkFromRow es)
                                (getTitleFromRow es)) (rows t1)
--       T.putStrLn (showT docs)
       return docs

parseDate :: Parser Day
parseDate = do
  d <- AP.decimal
  " "
  m <- parseMonth
  " "
  yr <- AP.decimal
  return $ fromGregorian yr m d

parseMonth :: Parser Int
parseMonth = do
  m <- AP.take 3
  return $
    if m=="Jan" then 1
    else if m=="Feb" then 2
         else if m=="Mar" then 3
              else if m=="Apr" then 4
                   else if m=="May" then 5
                        else if m=="Jun" then 6
                             else if m=="Jul" then 7
                                  else if m=="Aug" then 8
                                       else if m=="Sep" then 9
                                           else if m=="Oct" then 10
                                                else if m=="Nov" then 11
                                                      else if m=="Dec" then 12
                                                           else error $ "I don't know the month "++(T.unpack m)

getDateFromRow tags = let ds = deleteAll ["\n","\t","\r"] $ fromTagText (head (head tags))
                      in case AP.parseOnly parseDate ds of
                        Left m -> error $ "Parse error while processing a date: "++m
                        Right d -> d

getTypeFromRow tags = let tag = (tags!!1)!!3
                      in if isTagOpen tag
                            then let attr = fromAttrib "title" tag
                                 in if attr=="" then Nothing else Just (textToBISDocType attr)
                         else Nothing

getLinkFromRow tags = let tag = (tags!!2)!!3
                      in if isTagOpen tag
                         then Just $ T.concat [bisSite,fromAttrib "href" tag]
                         else Nothing

getTitleFromRow tags = let tag = (tags!!2)!!4
                       in if isTagText tag
                          then deleteAll ["\t","\r","\n"] $ fromTagText tag
                          else ""

deleteAll (s:ss) text = deleteAll ss (T.replace s "" text)
deleteAll [] text = text

getNextBISDocPage :: [TTag] -> IO (Maybe [TTag])
getNextBISDocPage tags = do
  let nextLink = getNextDocLink tags
  case nextLink of
    Just lnk -> do
      putStrLns ["Follow link: ",lnk]
      src <- openUrlUtf8 lnk
      let tags1 = parseTags src
      return (Just tags1)
    Nothing -> return Nothing

getNextDocLink :: [TTag] -> Maybe Text
getNextDocLink tags =
  let s = sections (~== (TagOpen ("a"::Text) [("class","next")])) tags
  in if null s
     then Nothing             
     else let t = head (head s)
          in let href = fromAttrib "href" t
             in if href==""
                then Nothing
                else Just (if T.head href=='/'
                           then T.concat [bisSite,href]
                           else href)

data Table = Table {
  header :: TableRow,
  rows :: [TableRow]
  } deriving (Show)

data TableRow = TableRow {
  elements :: [[TTag]]
  } deriving (Show)

-- table should have a simple rectangualr structure and only on header row and at least one body row.
simpleTables :: [TTag] -> [Table]
simpleTables tags =
  let ts = partitions (~== (TagOpen ("table"::Text) [])) tags
      rows = map tableRows ts
  in map (\rows -> Table (head rows) (tail rows)) rows

tableHeader :: [TTag] -> [TableRow]
tableHeader tableTags = 
  let thead = partitions (~== (TagOpen ("thead"::Text) [])) tableTags
  in if null thead
     then []
     else tableRows (head thead)

tableRows :: [TTag] -> [TableRow]
tableRows trTags =
  let trs = partitions (~== (TagOpen ("tr"::Text) [])) trTags
  in if null trs
     then []
     else map tableRow trs

tableRow :: [TTag] -> TableRow
tableRow colTags =
  let tds = partitions (~== (TagOpen ("td"::Text) [])) colTags
  in if null tds
     then TableRow [[]]
     else TableRow $ map tableElement tds

skipLast l = take (n-1) l
  where n = length l

tableElement tdTags = skipLast (tail tdTags)

analyzeBisSingleDocPage :: Text -> IO ()
analyzeBisSingleDocPage url = do
  allTags <- return . partitionIt =<< (return . parseTags) =<< openUrlUtf8 url
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
           T.putStrLn docTitle
           T.putStrLn (showT docSummary)
           let fullTextLink = let fBox = sections (~==divFullText) annotationTags
                              in if length fBox>=1
                                 then let aTag = sections (~==aLinkTag) (fBox!!0)
                                          link = fromAttrib "href" (head $ head aTag)
                                      in if "/" `T.isPrefixOf` link
                                         then Just $ T.concat [bisSite,link]
                                         else Just link
                                 else Nothing
           T.putStrLn (showT fullTextLink)
           let otherBoxes = partitions (~== otherBox) annotationTags
           let others = map processOtherBox otherBoxes
           mapM_ (\x -> T.putStrLn (showT x)) others
           return ()
         else do
           T.putStrLn "Cannot parse page because structure has changed.\n"
           putStrLns ["Content:\n",showT allTags]
    else do
      T.putStrLn "No input. Probably You are not connected to the internet.\n"
    
  where tagContent = TagOpen "h1" [] :: TTag
        tagAnnotations = TagOpen "div" [("id","right"),
                                        ("class","column"),
                                        ("role","complementary")] :: TTag
        tagFooter = TagOpen "div" [("id","footer"),("role","contentinfo")] :: TTag
        partitionIt tags = partitions (\tag -> tag~==tagContent
                                               || tag~==tagAnnotations
                                               || tag~==tagFooter) tags
        divFullText = TagOpen "div" [("class","list_box full_text")] :: TTag
        aLinkTag = TagOpen "a" [] :: TTag
        otherBox = TagOpen "div" [("class","list_box")] :: TTag
        processOtherBox tags = let t1 = sections (~== (TagOpen "h4" [] :: TTag)) tags
                                   typ = fromTagText ((head t1)!!1)
                                   t2 = sections (~==aLinkTag) tags
                                   link' = fromAttrib "href" (head $ head t2)
                                   link = if "/" `T.isPrefixOf` link'
                                          then T.concat [bisSite,link']
                                          else link'
                               in (typ,link)


purgeHtmlText :: [TTag] -> [TTag]
purgeHtmlText tags = successively [map replaceWhiteSpaceInText,
                                   filter (\t -> not (isTagDiv t) && not (isEmptyTagText t))]
                     tags

isEmptyTagText :: TTag -> Bool
isEmptyTagText tag = if isTagText tag
                     then let (TagText text) = tag
                          in text==""
                     else False

isTagDiv :: TTag -> Bool
isTagDiv (TagOpen "div" _) = True
isTagDiv (TagClose "div") = True
isTagDiv _ = False

replaceWhiteSpaceInText :: TTag -> TTag
replaceWhiteSpaceInText tag = if isTagText tag
                              then let (TagText text) = tag
                                   in TagText $ successively
                                      [T.replace "\t" " ",
                                       T.replace "\n" " ",
                                       T.replace "\r" " ",
                                       T.replace "      " " ",
                                       T.replace "     " " ",
                                       T.replace "    " " ",
                                       T.replace "   " " ",
                                       T.replace "  " " ",
                                       \t -> if T.isPrefixOf " " t then T.tail t else t]
                                      text
                              else tag

successively :: [a -> a] -> a -> a
successively (f:fs) x0 = let x1 = f x0
                         in successively fs x1
successively [] x = x
