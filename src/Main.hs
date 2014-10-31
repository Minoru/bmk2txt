module Main where

import Control.Lens hiding (argument)
import Control.Monad
import Data.Attoparsec.Text
import System.Console.Docopt
import System.Environment
import System.Exit
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Version as V

import Bookmark
import Parsers
import Paths_bmk2txt (version)
import Usage

main :: IO ()
main = do
  arguments <- getArgs
  args <- (if null arguments then withArgs ["--help"] else id)
          (return arguments >>= optionsWithUsage usage)

  when (args `isPresent` (longOption "help")) $ do
    TIO.putStrLn $ T.pack usage
    exitWith ExitSuccess

  when (args `isPresent` (longOption "version")) $ do
    putStrLn ("bmk2txt " ++ V.showVersion version)
    exitWith ExitSuccess

  let doStrip = args `isPresent` (longOption "strip")
  mapM_ (process doStrip) $ args `getAllArgs` (argument "FILE")

process :: Bool -> FilePath -> IO ()
process do_strip path = do
  file <- TIO.readFile path
  case parseOnly pBookmarksFile file of
    Left err -> print err
    Right res ->
      forM_ res $ print . (if do_strip then stripchars else id)

stripchars :: Bookmark -> Bookmark
stripchars = over text (T.dropAround (`elem` " .,!?“”«»‘’—–- ()[]:;"))
