{-# LANGUAGE OverloadedStrings,RecordWildCards,TypeSynonymInstances,DeriveDataTypeable #-}
module Data.CrawlerParameters (
  CrawlParams(..),MongoParams(..),
  _PROGRAM_NAME,_PROGRAM_VERSION,_PROGRAM_INFO,_PROGRAM_ABOUT,_COPYRIGHT,
  processParams
  ) where

import           Control.Monad
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (Day,NominalDiffTime)
import           Data.Word (Word16)
import qualified Database.Persist.MongoDB as Mongo
import           Network (PortID (PortNumber))
import           System.Console.CmdArgs.Implicit
import           System.Exit (ExitCode(..),exitWith)

data CrawlParams = CrawlParams {
  courtesyPeriod :: Int,    -- ^Courtesy period in micro seconds
  pdfPath :: FilePath,
  publishedFrom :: Maybe Day,
  publishedUntil :: Maybe Day}
                 deriving (Show)

data MongoParams = MongoParams {
  db :: Mongo.Database,
  host :: Mongo.HostName,
  port :: PortID,
  auth :: Maybe Mongo.MongoAuth,
  dt :: NominalDiffTime,
  mode :: Mongo.AccessMode}
                 deriving Show

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
    def &= help "Only report publications not before this date (default: no restriction, format: YYYY-MM-DD)"
    &= typ "DAY"
    &= explicit &= name "from" &= name "f",
  paramUntil =
    def &= help "Only report publications not after this date (default: no restriction, format: YYYY-MM-DD)"
    &= typ "DAY"
    &= explicit &= name "until" &= name "u",
  paramPdfPath =
    def &= help "Output path for pdf files (default ./)"
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
_PROGRAM_VERSION = "0.2"
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

processParams :: IO (MongoParams, CrawlParams)
processParams = do
  params <- cmdArgs appParams
  let ct = let x = paramCourtesyPeriod params
           in if x==0 then 2000000 else fromIntegral x
  let t0 = fromString (paramFrom params) :: Maybe Day
  let t1 = fromString (paramUntil params) :: Maybe Day
  let db = T.pack $ let x = paramMongoDb params
                    in if null x then "BCBS" else x
  let host = let h = paramMongoHost params
             in if null h then "127.0.0.1" else h
  let port = let p = fromIntegral $ paramMongoPort params
             in if p==0 then PortNumber 27017 else PortNumber p
  let auth = if null (paramMongoUser params)
             then Nothing
             else Just (Mongo.MongoAuth (T.pack $ paramMongoUser params)
                        (T.pack $ paramMongoPassword params))
  let dt = let x = paramMongoNominalDiffTime params
           in if x==0 then 2000 else fromIntegral x
  when (dt<=0) $ do
    putStrLn "Nominal diff time must be positive!"
    exitWith (ExitFailure 1)
  putStr "Crawl www.bis.org for publications"
  case t0 of
    Just t0' -> case t1 of
      Just t1' -> putStrLn $ " between "++show t0'++" and "++show t1'
      Nothing -> putStrLn $ " after "++show t0'
    Nothing -> case t1 of
      Just t1' -> putStrLn $ " before "++show t1'
      Nothing -> putStrLn ""
  putStrLn $ "Courtesy period: "++show (1e-6*fromIntegral ct)++"s"
  putStrLn ""
  putStrLn $ "Connect to MongoDB "++T.unpack db++" at "++host++":"++show port
  putStrLn $ "MongoDB authentication: "++show auth
  putStrLn $ "Nomminal diff time: "++show dt
  putStrLn ""
  let mongoP = MongoParams{mode=Mongo.master,..}
  let crawlP = CrawlParams{courtesyPeriod=ct,
                           pdfPath=paramPdfPath params,
                           publishedFrom=t0,
                           publishedUntil=t1}
  return (mongoP,crawlP)
