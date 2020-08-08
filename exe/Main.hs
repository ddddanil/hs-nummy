module Main where

import Nummy.Prelude
import qualified Data.Text as T
import System.Console.Haskeline
import System.Console.ANSI

import Nummy.Parser
import Application.Repl


-- Argument parsing

args :: IO Text
args = T.pack . intercalate " " <$> getArgs

tryArgs :: IO () -> IO ()
tryArgs fallback = do
  a <- args
  if T.null a
    then fallback
    else print =<< nummy a


-- Repl types

haskeline_repl :: IO ()
haskeline_repl = runInputT defaultSettings loop where
  loop = do
    minput <- fmap T.pack <$> getInputLine "> "
    case minput of
      Nothing -> return ()
      Just input -> do
        output <- liftIO $ nummy input
        print $ output
        loop

custom_repl :: IO ()
custom_repl = repl nummy


-- Main

main :: IO ()
main =
  tryArgs $ do
    fancy <- hSupportsANSI stdout
    case fancy of
      True -> custom_repl
      False -> haskeline_repl
