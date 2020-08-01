module Main where

import Nummy.Prelude
import qualified Data.Text as T (pack, unpack)
import Text.Megaparsec
import System.Console.Haskeline

import Nummy.Parser (parse_nummy)
import Nummy.Metrology (runReadUnit)

runNummy :: String -> IO (Either (ParseErrorBundle Text Void) Text)
runNummy i = runReadUnit (runParserT parse_nummy "<input>" (T.pack i))

main :: IO ()
main = runInputT defaultSettings loop where
  loop = do
    minput <- getInputLine "> "
    case minput of
      Nothing -> return ()
      Just input -> do
        output <- liftIO $ runNummy input
        case output of
          Left err -> outputStrLn $ errorBundlePretty err
          Right res -> outputStrLn (T.unpack res)
        loop
