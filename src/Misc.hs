{-# LANGUAGE OverloadedStrings,PackageImports #-}
module Misc where

import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as AP
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import qualified Data.Text as T (concat,pack,replace,empty,unpack,isPrefixOf,tail)
import qualified Data.Text.IO as T (putStrLn)
import           Data.Time (Day,fromGregorian)
import "network" Network.URI (parseURI,uriPath)
import           System.FilePath.Posix (splitFileName)
import           Text.HTML.TagSoup
import qualified Text.Pandoc as P

showMaybe :: Show a => Maybe a -> String
showMaybe (Just x) = show x
showMaybe Nothing = ""

putStrLns :: [Text] -> IO ()
putStrLns = T.putStrLn . T.concat

showT :: (Show s) => s -> Text
showT = T.pack . show

between :: Ord a => a -> Maybe a -> Maybe a -> Bool
between t (Just t0) (Just t1) = t>=t0 && t<=t1
between t Nothing (Just t1) = t<=t1
between t (Just t0) Nothing = t>=t0
between t Nothing Nothing = True

deleteAll :: [Text] -> Text -> Text
deleteAll (s:ss) text = deleteAll ss (T.replace s T.empty text)
deleteAll [] text = text

compose :: [a -> a] -> a -> a
compose (f:fs) x0 = let x1 = f x0
                         in compose fs x1
compose [] x = x

filenameFromUrl = snd . splitFileName . uriPath . fromJust . parseURI

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


aLinkTag :: Tag Text
aLinkTag = TagOpen "a" []

purgeHtmlText :: [Tag Text] -> [Tag Text]
purgeHtmlText = compose [map replaceWhiteSpaceInText,
                                   filter (\t -> not (isTagDiv t) && not (isEmptyTagText t))]

isEmptyTagText :: Tag Text -> Bool
isEmptyTagText tag = isTagText tag && (let (TagText text) = tag
                                       in text == "")

isTagDiv :: Tag Text -> Bool
isTagDiv (TagOpen "div" _) = True
isTagDiv (TagClose "div") = True
isTagDiv _ = False

replaceWhiteSpaceInText :: Tag Text -> Tag Text
replaceWhiteSpaceInText tag = if isTagText tag
                              then let (TagText text) = tag
                                   in TagText $ compose
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

htmlToMarkdown :: Text -> Text
htmlToMarkdown = T.pack . P.writeMarkdown P.def . P.readHtml P.def . T.unpack

