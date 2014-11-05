{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.Crawler (
  module Control.Monad.Reader,
  module Control.Monad.Writer.Lazy,
  module Control.Monad.Catch,
  module Control.Monad.Error,
  module Network.Curl,
  module Mongo,
  Crawler,runCrawler,logMsg,liftMongo,openUrlUtf8',getFile'
  ) where

import           Data.CrawlerParameters
import           Data.Crawler

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad.Catch
import           Control.Monad.Error
import           Control.Monad.Reader
import           Control.Monad.Writer.Lazy
import           Data.ByteString (ByteString)
import           Data.Text (Text)
import qualified Data.Text.Encoding as T (decodeUtf8)
import qualified Database.Persist.MongoDB as Mongo
import           Network.Curl

newtype Crawler a = C {
  runC :: ErrorT CrawlError (WriterT [CrawlMessage] (ReaderT CrawlParams (Mongo.Action IO))) a
  } deriving (Functor,Applicative,Monad,MonadError CrawlError,
              MonadThrow,MonadCatch,MonadIO,
              MonadReader CrawlParams,MonadWriter [CrawlMessage])

runCrawler :: MongoParams -> CrawlParams -> Crawler a -> IO (Either CrawlError a, [CrawlMessage])
runCrawler mongoP crawlP crwlr = withCurlDo withinCurl
  where withinCurl =
          (Mongo.withMongoDBConn (db mongoP) (host mongoP) (port mongoP)
                                 (auth mongoP) (dt mongoP) $ \pool -> do
              Mongo.runMongoDBPool (mode mongoP) withinMongoDB pool)
          `catch` (\e -> do
                      let msg = "There is a problem with MongoDB: "++show (e :: SomeException)
                      putStrLn $ "bcbs-crawl: "++msg
                      return (Left (CrawlError msg),[]))
        withinMongoDB =
          (runReaderT (runWriterT (runErrorT (runC crwlr))) crawlP)
          `catch` (\e -> do
                      let msg = "Global exception: "++show (e :: SomeException)
                      liftIO $ putStrLn $ "bcbs-crawl: "++msg
                      return (Left (CrawlError msg),[]))

logMsg :: Text -> Crawler ()
logMsg msg = C $ do
  tell [CrawlMsg msg]

liftMongo :: Mongo.Action IO a -> Crawler a
liftMongo = C . lift . lift . lift

openUrlUtf8' :: URLString -> Crawler Text
openUrlUtf8' url = do
  resp <- liftIO $ (curlGetResponse_ url []
                    :: IO (CurlResponse_ [(String,String)] ByteString))
  ct <- courtesyPeriod <$> ask
  liftIO $ threadDelay ct
  return (T.decodeUtf8 (respBody resp))

getFile' :: URLString -> Crawler ByteString
getFile' url = do
  ct <- courtesyPeriod <$> ask
  liftIO $ do
    resp <- curlGetResponse_ url [] :: IO (CurlResponse_ [(String,String)] ByteString)
    threadDelay ct
    return $ respBody resp
