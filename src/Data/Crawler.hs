{-# LANGUAGE OverloadedStrings,GADTs,DeriveDataTypeable,DeriveFunctor,GeneralizedNewtypeDeriving,MultiParamTypeClasses,QuasiQuotes,TemplateHaskell,TypeFamilies,PackageImports,NamedFieldPuns,RecordWildCards,TypeSynonymInstances,FlexibleContexts #-}
module Data.Crawler where

import qualified Control.Exception as Ex
import           Control.Monad.Catch
import           Control.Monad.Error
import           Data.Text (Text)
import           Data.Time
import           Data.Typeable
import           Database.Persist
import qualified Database.Persist.MongoDB as Mongo
import           Database.Persist.TH
import           Language.Haskell.TH.Syntax (Type(ConT))
import           Network.Curl

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

data CrawlError = CrawlError String
                deriving Show

data MyException = ThisException | ThatException
                 deriving (Show, Typeable)

instance Exception MyException

instance Error CrawlError where
    noMsg  = CrawlError "Oh no!"
    strMsg = CrawlError

data CrawlMessage = CrawlIOErr Text (Maybe Int) | CrawlMsg Text
                  deriving (Show)

