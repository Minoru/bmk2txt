{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Usage (
  usage
) where

import ToStringQuasiQuoter (toString)

usage :: String
usage = [toString|Convert CoolReader's bookmark files to simple text files.
Usage:
    bmk2txt -h
    bmk2txt -V
    bmk2txt FILE ...

Options:
    -h --help     Show this message
    -V --version  Show program version
|]
