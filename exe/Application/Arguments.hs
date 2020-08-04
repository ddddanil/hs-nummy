module Application.Arguments where

import Nummy.Prelude
import qualified Data.Text as T
import Application.Parser

args :: IO Text
args = T.pack . intercalate " " <$> getArgs

tryArgs :: IO () -> IO ()
tryArgs fallback = do
  a <- args
  if T.null a
    then fallback
    else putStrLn =<< runParser prettyParserError a
