{-# LANGUAGE OverloadedStrings #-}

module Bookmark (
  Bookmark(..)
) where

import qualified Data.Text as T

data Bookmark = Bookmark {
    text    :: T.Text
  , comment :: T.Text
  }

instance Show Bookmark where
  show bmk = T.unpack $ if T.null (comment bmk)
    then text bmk
    else T.concat [ text bmk, " (", comment bmk, ")" ]
