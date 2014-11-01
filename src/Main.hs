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

  mapM_ (process (args^.strip)) (map T.unpack $ args^.files)

process :: Bool -> FilePath -> IO ()
process do_strip path = do
  file <- TIO.readFile path
  case parseOnly pBookmarksFile file of
    Left err -> print err
    Right res ->
      forM_ res $ print . (if do_strip then stripchars else id)

stripchars :: Bookmark -> Bookmark
stripchars = over text (T.dropAround (`elem` charsToStrip))
