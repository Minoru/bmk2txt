{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad

main :: IO ()
main = do
  file <- TIO.readFile "./test.bmk.txt"
  print $ parseOnly pBookmarksFile file

data Bookmark = Bookmark {
    text    :: T.Text
  , comment :: T.Text
  }

instance Show Bookmark where
  show bmk = T.unpack $ if not . T.null $ comment bmk
    then T.concat [ text bmk, " (", comment bmk, ")" ]
    else text bmk

pBookmarksFile :: Parser [Bookmark]
pBookmarksFile = do
  pBom
  pBookmarksHeader
  return [] -- bmks

pBom :: Parser ()
pBom = void $ string "\xfeff"

pBookmarksHeader :: Parser ()
pBookmarksHeader = void $ do
  string "# Cool Reader 3 - exported bookmarks" >> endOfLine
  string "# file name: " >> skipWhile (not . isEndOfLine) >> endOfLine
  string "# file path: " >> skipWhile (not . isEndOfLine) >> endOfLine
  string "# book title: " >> skipWhile (not . isEndOfLine) >> endOfLine
  string "# author: " >> skipWhile (not . isEndOfLine) >> endOfLine
  string "# series: " >> skipWhile (not . isEndOfLine) >> endOfLine
  endOfLine
