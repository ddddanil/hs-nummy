module Main where

import Nummy.Prelude
import qualified Data.Text as T

import Application.Repl


-- Argument parsing

args :: IO Text
args = T.pack . intercalate " " <$> getArgs


-- Main

main :: IO ()
main = do
  a <- args
  when (T.null a) $ repl
  rep a
