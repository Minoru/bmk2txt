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
  show bmk = T.unpack $ if T.null (comment bmk)
    then text bmk
    else T.concat [ text bmk, " (", comment bmk, ")" ]

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
  "# file name: " *> pSkipLine >> endOfLine
  "# file path: " *> pSkipLine >> endOfLine
  "# book title: " *> pSkipLine >> endOfLine
  "# author: " *> pSkipLine >> endOfLine
  "# series: " *> pSkipLine >> endOfLine
  endOfLine

pBookmark :: Parser Bookmark
pBookmark = do
  -- ## %pos% - comment
  string "## "
  _ <- double
  string "% - comment"
  endOfLine

  -- ## %title%
  string "## "
  pSkipLine
  endOfLine

  -- << %text%
  string "<< "
  text <- pLine
  endOfLine

  -- >> %comment%
  string ">> "
  comment <- pLine
  endOfLine

  -- extra newline at the end
  endOfLine

  return $ Bookmark text comment

pLine :: Parser T.Text
pLine = takeWhile (not . isEndOfLine)

pSkipLine :: Parser ()
pSkipLine = skipWhile (not . isEndOfLine)
