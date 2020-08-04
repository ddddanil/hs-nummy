module Application.Base where

import Nummy.Prelude
import qualified Data.Text as T (pack, unpack)
import System.Console.Haskeline

import Application.Parser
import Application.Repl

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
