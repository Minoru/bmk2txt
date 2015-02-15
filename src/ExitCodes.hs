module ExitCodes (
  errParseFail
) where

import System.Exit

errParseFail :: ExitCode
errParseFail = ExitFailure 1
