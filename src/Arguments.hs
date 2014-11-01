{-# LANGUAGE TemplateHaskell #-}

module Arguments (
  Arguments(..)
-- Arguments' lens
, strip
, stripChars
, stripCharsAdditional
, help
, version
, files
) where

import Control.Lens hiding (argument)
import qualified Data.Text as T

data Arguments = Arguments {
    _strip                :: Bool
  , _stripChars           :: T.Text
  , _stripCharsAdditional :: T.Text
  , _help                 :: Bool
  , _version              :: Bool
  , _files                :: [T.Text]
  }
  deriving (Show)

makeLenses ''Arguments
