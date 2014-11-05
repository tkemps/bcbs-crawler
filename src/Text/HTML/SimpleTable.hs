{-# LANGUAGE OverloadedStrings #-}
module Text.HTML.SimpleTable where

import           Text.HTML.TagSoup
import           Data.Text (Text)

data Table = Table {
  header :: TableRow,
  rows :: [TableRow]
  } deriving (Show)

data TableRow = TableRow {
  elements :: [[Tag Text]]
  } deriving (Show)

-- table should have a simple rectangular structure and only one header row and at
-- least one body row.
simpleTables :: [Tag Text] -> [Table]
simpleTables tags =
  let ts = partitions (~== (TagOpen ("table"::Text) [])) tags
      rows = map tableRows ts
  in map (\rows -> Table (head rows) (tail rows)) rows

tableHeader :: [Tag Text] -> [TableRow]
tableHeader tableTags = 
  let thead = partitions (~== (TagOpen ("thead"::Text) [])) tableTags
  in if null thead
     then []
     else tableRows (head thead)

tableRows :: [Tag Text] -> [TableRow]
tableRows trTags =
  let trs = partitions (~== (TagOpen ("tr"::Text) [])) trTags
  in if null trs
     then []
     else map tableRow trs

tableRow :: [Tag Text] -> TableRow
tableRow colTags =
  let tds = partitions (~== (TagOpen ("td"::Text) [])) colTags
  in if null tds
     then TableRow [[]]
     else TableRow $ map tableElement tds

skipLast l = take (n-1) l
  where n = length l

tableElement tdTags = skipLast (tail tdTags)
