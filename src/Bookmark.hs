{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Bookmark (
  Bookmark(..)
-- Bookmark's lens
, text
, comment
) where

import Control.Lens
import qualified Data.Text as T

data Bookmark = Bookmark {
    _text    :: T.Text
  , _comment :: T.Text
  }

makeLenses ''Bookmark

instance Show Bookmark where
  show bmk = T.unpack $ if T.null (bmk^.comment)
    then bmk^.text
    else T.concat [ bmk^.text, " (", bmk^.comment, ")" ]
