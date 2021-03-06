module Main where

import Control.Lens hiding (argument)
import Control.Monad
import Data.Attoparsec.Text
import System.Console.CmdArgs.Explicit hiding (process)
import System.Exit
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Version as V

import Arguments
import Bookmark
import ExitCodes
import Parsers
import Usage
import qualified Paths_bmk2txt as P

main :: IO ()
main = do
  args <- processArgs usage

  -- Show help in either of those cases:
  -- * --help is passed
  -- * --version is not passed and no files are specified
  when (args^.help || (not $ args^.version) && (null $ args^.files)) $ do
    print $ helpText [] HelpFormatDefault usage
    exitWith ExitSuccess

  when (args^.version) $ do
    putStrLn ("bmk2txt " ++ V.showVersion P.version)
    exitWith ExitSuccess

  mapM_
    (process
      (args^.strip)
      (args^.stripChars)
      (args^.stripCharsAdditional)
      (args^.zero_terminated))
    (map T.unpack $ args^.files)

process :: Bool -> T.Text -> T.Text -> Bool -> FilePath -> IO ()
process do_strip specific additional zero_terminated path = do
  file <- TIO.readFile path
  case parseOnly pBookmarksFile file of
    Left err -> do
      print err
      exitWith errParseFail
    Right res -> do
      let chars = if T.null specific
          then charsToStrip ++ T.unpack additional
          else T.unpack specific
      forM_ res $ printOut . (if do_strip then stripchars chars else id)

  where
    printOut = if zero_terminated
      then \x -> putStr $ (show x) ++ "\0"
      else print

stripchars :: [Char] -> Bookmark -> Bookmark
stripchars chars = over text (T.dropAround (`elem` chars))
