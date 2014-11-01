{-# LANGUAGE OverloadedStrings #-}

module Usage (
  usage
, charsToStrip
) where

import Control.Lens hiding (argument)
import System.Console.CmdArgs.Explicit
import qualified Data.Text as T

import Arguments

charsToStrip :: String
charsToStrip = ".,:;!?“”«»‘’—–- ()[]"

usage = mode
  "bmk2txt"
  Arguments {
      _strip                = False
    , _help                 = False
    , _version              = False
    , _files                = []
    }
  "Convert CoolReader's bookmark files to simple text files"
  (flagArg (\f args -> Right $ over files ((T.pack f) : ) args) "FILE ...")
  [ flagNone
      ["strip", "s"]
      (strip .~ True)
      ("Strip characters from the beginning and the end of the bookmarked text. The following ones are stripped by default: " ++ charsToStrip)

  , flagNone
      ["help", "h"]
      (help .~ True)
      "Show this message"
  , flagNone
      ["version", "V"]
      (version .~ True)
      "Show program version"
  ]
