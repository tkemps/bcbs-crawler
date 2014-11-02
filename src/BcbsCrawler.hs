{-# LANGUAGE OverloadedStrings,NamedFieldPuns,DeriveDataTypeable,GADTs,
             GeneralizedNewtypeDeriving,MultiParamTypeClasses,QuasiQuotes,TemplateHaskell,
             TypeFamilies,PackageImports,TypeSynonymInstances #-}
module Main where

import           Control.Applicative
import           Control.Concurrent
import qualified Control.Exception as Ex
import           Control.Monad
import           Control.Monad.IO.Class
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
import           Language.Haskell.TH.Syntax
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
type BisDocType = Text
type LinkType = Text

let mongoSettings = (mkPersistSettings (ConT ''Mongo.MongoContext)) {mpsGeneric = False}
 in share [mkPersist mongoSettings] [persistLowerCase|
BisDoc
  date Day
  typ BisDocType Maybe
  link URLString Maybe
  title Text
  details BisDocumentDetails Maybe
  deriving Show
BisDocumentDetails
  documentTitle Text
  summary Text
  fullTextLink URLString Maybe
  fullTextMarkdown Text Maybe
  localFile FilePath Maybe
  otherLinks [DocumentLink]
  deriving Show
DocumentLink
  linkType LinkType
  documentLinks [URLString]
  deriving Show
|]

data AppParams = AppParams {
  paramCourtesyPeriod :: Int,
  paramFrom :: String,
  paramUntil :: String,
  paramPdfPath :: FilePath,
  paramMongoDb :: String,
  paramMongoHost :: String,
  paramMongoPort :: Word16,
  paramMongoUser :: String,
  paramMongoPassword :: String,
  paramMongoNominalDiffTime :: Int
  } deriving (Show,Data,Typeable)

instance Default Mongo.Database where
  def = ""

appParams = AppParams {
  paramCourtesyPeriod =
     def &= help "Courtesy period in micro seconds between successive accesses to the web site (default 2s)"
     &= explicit &= name "courtesyperiod",
  paramFrom =
    def &= help "Only report publications not before this date"
    &= typ "DAY"
    &= explicit &= name "from" &= name "f",
  paramUntil =
    def &= help "Only report publications not after this date"
    &= typ "DAY"
    &= explicit &= name "until" &= name "u",
  paramPdfPath =
    def &= help "Output path for pdf files"
    &= opt ("./pdf/"::String)
    &= typDir
    &= explicit &= name "pdfpath" &= name "p",
  paramMongoDb =
    def &= help "Name of the MongoDB database where to write the publications to (default 'BIS')"
    &= explicit &= name "db",
  paramMongoHost =
    def &= help "Host of the MongoDB database (default 127.0.0.1)"
    &= explicit &= name "host",
  paramMongoPort =
    def &= help "Port of the MongoDB host (default 27017)"
    &= explicit &= name "port",
  paramMongoUser =
    def &= help "User name for MongoDB (default: no authentication)"
    &= explicit &= name "user",
  paramMongoPassword =
    def &= help "Password for MongoDB"
    &= explicit &= name "password",
  paramMongoNominalDiffTime =
    def &= help "Time a connection is left idle before closing (default 2000)"
    &= explicit &= name "nominaldifftime"
  }
            &= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]
            &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
            &= help _PROGRAM_ABOUT
            &= helpArg [explicit, name "help", name "h"]
            &= program _PROGRAM_NAME

_PROGRAM_NAME = "bcbs-crawl"
_PROGRAM_VERSION = "0.1"
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_PROGRAM_ABOUT = "Crawls the BCBS publications at www.bis.org"
_COPYRIGHT = "(C) Torsten Kemps-Benedix 2014"

fromString :: Read a => String -> Maybe a
fromString s = if s==""
                  then Nothing
                  else let prs = reads s
                       in if null prs
                          then Nothing
                          else Just (fst (head prs))

main :: IO ()
main = do
  params <- cmdArgs appParams
  putStrLn _PROGRAM_INFO
  let ct = let x = paramCourtesyPeriod params
           in if x==0 then 2 else fromIntegral x
  let t0 = (fromString (paramFrom params)) :: Maybe Day
  let t1 = (fromString (paramUntil params)) :: Maybe Day
  let db = T.pack $ let x = paramMongoDb params
                    in if null x then "BIS" else x
  let host = let h = paramMongoHost params
             in if null h then "127.0.0.1" else h
  let portId = let p = fromIntegral $ paramMongoPort params
               in if p==0 then PortNumber 27017 else PortNumber p
  let mongoAuth = if null (paramMongoUser params)
                  then Nothing
                  else Just (Mongo.MongoAuth (T.pack $ paramMongoUser params)
                             (T.pack $ paramMongoPassword params))
  let dt = let x = paramMongoNominalDiffTime params
           in if x==0 then 2000000 else fromIntegral x
  when (dt<=0) $ do
    putStrLn "Nominal diff time must be positive!"
    exitWith (ExitFailure 1)
  putStr "Crawl www.bis.org for publications"
  case t0 of
    Just t0' -> case t1 of
      Just t1' -> putStrLn $ " between "++show t0'++" and "++show t1'++"."
      Nothing -> putStrLn $ " after "++show t0'++"."
    Nothing -> case t1 of
      Just t1' -> putStrLn $ " before "++show t1'++"."
      Nothing -> putStrLn $ "."
  putStrLn $ "Courtesy period: "++show ct++" micro seconds."
  putStrLn ""
  docs <- crawlBisBcbsPublications ct t0 t1
  putStrLn ""
  putStrLn $ "Connect to MongoDB "++T.unpack db++" at "++host++":"++show portId
  putStrLn $ "MongoDB authentication: "++show mongoAuth
  putStrLn $ "Nomminal diff time: "++show dt
  putStrLn ""
  Ex.catch
    (do
      Mongo.withMongoDBConn db host portId mongoAuth dt $ \pool -> do
        Mongo.runMongoDBPool Mongo.master (writeDocs ct docs (paramPdfPath params)) pool)
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
              
htmlToMarkdown = T.pack . P.writeMarkdown P.def . P.readHtml P.def . T.unpack

type TTag = Tag Text

filenameFromUrl = snd . splitFileName . uriPath . fromJust . parseURI

showMaybe (Just x) = show x
showMaybe Nothing = ""

putStrLns :: [Text] -> IO ()
putStrLns = T.putStrLn . T.concat

showT :: (Show s) => s -> Text
showT = T.pack . show

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

between :: Ord a => a -> Maybe a -> Maybe a -> Bool
between t (Just t0) (Just t1) = t>=t0 && t<=t1
between t Nothing (Just t1) = t<=t1
between t (Just t0) Nothing = t>=t0
between t Nothing Nothing = True

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
                                                           else error $ "I don't know the month "
                                                                ++T.unpack m

getDateFromRow tags = let ds = deleteAll ["\n","\t","\r"] $ fromTagText (head (head tags))
                      in case AP.parseOnly Main.parseDate ds of
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

deleteAll (s:ss) text = deleteAll ss (T.replace s "" text)
deleteAll [] text = text

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

data Table = Table {
  header :: TableRow,
  rows :: [TableRow]
  } deriving (Show)

data TableRow = TableRow {
  elements :: [[TTag]]
  } deriving (Show)

-- table should have a simple rectangular structure and only one header row and at
-- least one body row.
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

aLinkTag :: TTag
aLinkTag = TagOpen "a" []

purgeHtmlText :: [TTag] -> [TTag]
purgeHtmlText = successively [map replaceWhiteSpaceInText,
                                   filter (\t -> not (isTagDiv t) && not (isEmptyTagText t))]

isEmptyTagText :: TTag -> Bool
isEmptyTagText tag = isTagText tag && (let (TagText text) = tag
                                       in text == "")

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
