module Main where

import Nummy.Prelude
import qualified Data.Text as T (pack, unpack)
import Text.Megaparsec
import System.Console.Haskeline

import Nummy.Parser (parse_nummy)


main :: IO ()
main = runInputT defaultSettings loop where
  loop = do
    minput <- getInputLine "> "
    case minput of
      Nothing -> return ()
      Just input -> do
        case parse parse_nummy "<input>" (T.pack input) of
          Left err -> outputStrLn $ errorBundlePretty err
          Right res -> outputStrLn (T.unpack res)
        loop
