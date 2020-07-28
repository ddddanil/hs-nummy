module Main where

import Protolude
import Text.Parsec (parse, runParser)
import qualified Text.PrettyPrint.Leijen as PP
import System.Console.Haskeline

import Nummy.Parser (parse_nummy, Parser)


main :: IO ()
main = runInputT defaultSettings loop where
  loop = do
    minput <- getInputLine "> "
    case minput of
      Nothing -> return ()
      Just input -> do
        case runParser parse_nummy "" "" input of
          Left err -> outputStrLn $ show err
          Right qu -> outputStrLn $ show (PP.pretty qu)
        loop
