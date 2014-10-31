{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Usage (
  usage
) where

import ToStringQuasiQuoter (toString)

usage :: String
usage = [toString|Convert CoolReader's bookmark files to simple text files.
Usage:
    bmk2txt [--strip] FILE ...
    bmk2txt --help
    bmk2txt --version

Options:
    -s --strip    Strip whitespace and some punctuation characters from the
                  beginning and the end of the bookmarked text
    -h --help     Show this message
    -V --version  Show program version
|]
