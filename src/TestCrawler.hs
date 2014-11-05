{-# LANGUAGE OverloadedStrings,GADTs,DeriveDataTypeable,DeriveFunctor,GeneralizedNewtypeDeriving,MultiParamTypeClasses,QuasiQuotes,TemplateHaskell,TypeFamilies,PackageImports,NamedFieldPuns,RecordWildCards,TypeSynonymInstances,FlexibleContexts #-}
module Main where

import           Control.Monad.Crawler
import           Data.Crawler
import           Data.CrawlerParameters
import           Misc

import qualified Control.Exception as Ex
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Control.Monad.Writer.Lazy
import           Data.Time
import qualified Database.Persist.MongoDB as Mongo
import           Network (PortID (PortNumber))
import           Network.Curl

test :: Crawler a -> IO (Either CrawlError a, [CrawlMessage])
test testCrwlr = do
  let mongoP = MongoParams{db="BIS",
                           host="127.0.0.1",
                           port=PortNumber 27017,
                           auth=Nothing,
                           dt=2000,
                           mode=Mongo.master}
  let crawlP = CrawlParams{courtesyPeriod=2000000,
                           pdfPath="./pdf/",
                           publishedFrom=Nothing,
                           publishedUntil=Nothing}
  runCrawler mongoP crawlP testCrwlr

testCrwlr :: Crawler Int
testCrwlr = do
  logMsg "Hello!"
  throwM ThisException `catch` \e -> do
    liftIO $ putStrLn ("Caught " ++ show (e :: MyException))
    logMsg (showT e)
  (liftIO $ Ex.throwIO ThatException) `catch` \e -> do
    liftIO $ putStrLn ("Caught " ++ show (e :: MyException))
    logMsg (showT e)
  x <- (liftIO $ readFile "doesnotexist") `catch` \e -> do
    liftIO $ putStrLn ("Caught: " ++ show (e :: SomeException))
    logMsg (showT e)
    return "xxx"
  liftMongo $ Mongo.insert_ $ BisDoc {
    bisDocDate=fromGregorian 2014 3 16, bisDocTyp=Just "Junk", bisDocLink=Just "www.Nirvana.org",
    bisDocTitle="Nonsense",bisDocDetails=Nothing}
  p <- ask
  liftIO $ putStrLn $ "courtesyPeriod="++(show $ courtesyPeriod p)
  tell [CrawlMsg "This is a test message."]
  t <- openUrlUtf8' "http://www.un.org"
  tell [CrawlMsg t]
  return 42

main = do
     test testCrwlr
