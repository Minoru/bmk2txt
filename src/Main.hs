{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Prelude hiding (takeWhile)

main :: IO ()
main = do
  file <- TIO.readFile "./test.bmk.txt"
  case parseOnly pBookmarksFile file of
    Left err -> print err
    Right res -> mapM_ print res

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
  bmks <- many' pBookmark
  endOfInput
  return bmks

pBom :: Parser ()
pBom = void $ string "\xfeff"

pBookmarksHeader :: Parser ()
pBookmarksHeader = void $ do
  "# Cool Reader 3 - exported bookmarks" *> endOfLine
  "# file name: " *> skipWhile (not . isEndOfLine) >> endOfLine
  "# file path: " *> skipWhile (not . isEndOfLine) >> endOfLine
  "# book title: " *> skipWhile (not . isEndOfLine) >> endOfLine
  "# author: " *> skipWhile (not . isEndOfLine) >> endOfLine
  "# series: " *> skipWhile (not . isEndOfLine) >> endOfLine
  endOfLine

pBookmark :: Parser Bookmark
pBookmark = do
  -- ## %pos% - comment
  string "## "
  pos <- double
  string "% - comment"
  endOfLine

  -- ## %title%
  string "## "
  skipWhile (not . isEndOfLine)
  endOfLine

  -- << %text%
  string "<< "
  text <- takeWhile (not . isEndOfLine)
  endOfLine

  -- >> %comment%
  string ">> "
  comment <- takeWhile (not . isEndOfLine)
  endOfLine

  -- extra newline at the end
  endOfLine

  return $ Bookmark text comment
