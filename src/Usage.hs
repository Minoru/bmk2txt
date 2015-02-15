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
    , _stripChars           = ""
    , _stripCharsAdditional = ""
    , _zero_terminated      = False
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
  , flagReq
      ["strip-chars"]
      (\chars args -> Right $ (stripChars .~ T.pack chars) args)
      "CHARS"
      "Characters to strip"
  , flagReq
      ["strip-chars-additional"]
      (\chars args -> Right $ (stripCharsAdditional .~ T.pack chars) args)
      "CHARS"
      "Characters to strip, in addition to the default ones"
  , flagNone
      ["zero-terminated", "z"]
      (zero_terminated .~ True)
      "Delimit records with NUL, not newline"

  , flagNone
      ["help", "h"]
      (help .~ True)
      "Show this message"
  , flagNone
      ["version", "V"]
      (version .~ True)
      "Show program version"
  ]
