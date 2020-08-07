module Main where

import Nummy.Prelude
import qualified Data.Text as T (pack, unpack, null)
import System.Console.Haskeline
import System.Console.ANSI

import Application.Parser
import Application.Repl


-- Argument parsing

args :: IO Text
args = T.pack . intercalate " " <$> getArgs

tryArgs :: IO () -> IO ()
tryArgs fallback = do
  a <- args
  if T.null a
    then fallback
    else putStrLn =<< runParser prettyParserError a


-- Repl types

haskeline_repl :: IO ()
haskeline_repl = runInputT defaultSettings loop where
  loop = do
    minput <- fmap T.pack <$> getInputLine "> "
    case minput of
      Nothing -> return ()
      Just input -> do
        output <- liftIO $ runParser prettyParserError input
        putStrLn $ T.unpack output
        loop

custom_repl :: IO ()
custom_repl = repl (runParser symbolParserError)


-- Main

main :: IO ()
main =
  tryArgs $ do
    fancy <- hSupportsANSI stdout
    case fancy of
      True -> custom_repl
      False -> haskeline_repl
