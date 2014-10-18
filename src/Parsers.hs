{-# LANGUAGE OverloadedStrings #-}

module Parsers (
  pBookmarksFile
) where

import Prelude hiding (takeWhile)

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text
import qualified Data.Text as T

import Bookmark

pBookmarksFile :: Parser [Bookmark]
pBookmarksFile = do
  -- skip BOM if it's there, do nothing otherwise
  option () pBom
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
