module Katas.AlphabetCipher.Simple (
  Headers(..)
, Row(..)
, Table(..)
, TableDirection(..)
, makeTable
, makeSquareTable
, sliceBy
, findIn
, encode
, letters
, substitutions
, repeatedWord
, invertedFindIn
, decode
) where

import           Data.Char       (chr)
import           Data.List       (elemIndex)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromJust)

type Headers a = [a]
type Row a = (a, [a])
type Keyword = String
type CipherText = String

data Table a = Table {
    headers :: Headers a
  , rows    :: Map.Map a [a]
  , columns :: Map.Map a [a]
  } deriving (Show)

data TableDirection = RowDir | ColumnDir

makeTable :: Ord a => Headers a -> [Row a] -> Table a
makeTable hs rs = Table {
    headers = hs
  , rows    = Map.fromList rs
  , columns = Map.fromList $ map (rowOfHeader hs rs) hs
  }

makeRow :: a -> [a] -> Row a
makeRow rowId rowVals = (rowId, rowVals)

makeSquareTable :: Ord a => Headers a -> Table a
makeSquareTable hs = makeTable hs rows
  where rows = map (\ a -> (a, replicate (length hs) a)) hs

rowOfHeader :: Eq a => Headers a -> [Row a] -> a -> Row a
rowOfHeader hs rs h = (h, map (getAtIndex (indexOf h hs)) rs)

getAtIndex :: Int -> Row a -> a
getAtIndex el (_, els) = els !! el

indexOf :: Eq h => h -> Headers h -> Int
indexOf h hs = fromJust $ elemIndex h hs

sliceBy :: Ord a => TableDirection -> Table a -> a -> Maybe [a]
sliceBy RowDir t h = Map.lookup h $ rows t
sliceBy ColumnDir t h = Map.lookup h $ columns t

letters :: String
letters = map chr [97..122]

startingFrom :: Eq a => a -> [a] -> [a]
startingFrom el els = drop n els ++ take n els
  where n = indexOf el els

substitutions :: Table Char
substitutions = makeTable letters letterRows
  where letterRows = map (\l -> makeRow l $ startingFrom l letters) letters

repeatedWord :: String -> String
repeatedWord word = concat $ repeat word

findIn :: Ord a => Table a -> a -> a -> a
findIn table row column = rowVals !! colIndex
  where rowVals = fromJust $ sliceBy RowDir table row
        colIndex = indexOf column (headers table)

invertedFindIn :: Ord a => Table a -> a -> a -> a
invertedFindIn table column header = fst $ tableRowsList !! index
  where columnVals = fromJust $ sliceBy ColumnDir table header
        index = indexOf column columnVals
        tableRowsList = Map.toList $ rows table

encodeWith :: (Table Char -> Char -> Char -> Char) -> Keyword -> String -> String
encodeWith fn key text =
  map (uncurry $ fn substitutions) zipped
    where zipped = zip text repeatedKey
          repeatedKey = take (length text) $ repeatedWord key

encode :: Keyword -> String -> CipherText
encode = encodeWith findIn

decode :: Keyword -> CipherText -> String
decode = encodeWith invertedFindIn

