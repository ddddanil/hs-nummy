module Main where

import Nummy.Prelude
import qualified Data.Text as T (pack, unpack)
import Data.Bifoldable (bifold)
import Text.Megaparsec
import System.Console.Haskeline

import Nummy.Parser (nummy)
import Nummy.Cache (runReadCache)
import Repl


runNummy :: String -> IO Text
runNummy i = bifold <$> runReadCache (runExceptT $
  (T.pack . errorBundlePretty) `withExceptT` nummy (T.pack i))

haskeline_repl :: IO ()
haskeline_repl = runInputT defaultSettings loop where
  loop = do
    minput <- getInputLine "> "
    case minput of
      Nothing -> return ()
      Just input -> do
        outputStrLn =<< (liftIO $ T.unpack <$> runNummy input)
        loop


nummyAction :: ReplAction
nummyAction = replAction $ runReadCache . runExceptT <$>
  ( withExceptT (T.pack . errorBundlePretty) . nummy )

custom_repl :: IO ()
custom_repl = repl nummyAction


main :: IO ()
main = custom_repl
