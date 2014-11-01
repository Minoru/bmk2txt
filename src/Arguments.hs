{-# LANGUAGE TemplateHaskell #-}

module Arguments (
  Arguments(..)
-- Arguments' lens
, strip
, help
, version
, files
) where

import Control.Lens hiding (argument)
import qualified Data.Text as T

data Arguments = Arguments {
    _strip                :: Bool
  , _help                 :: Bool
  , _version              :: Bool
  , _files                :: [T.Text]
  }
  deriving (Show)

makeLenses ''Arguments
