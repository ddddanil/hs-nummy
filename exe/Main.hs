{-# LANGUAGE FlexibleContexts #-}
module Main where

import Protolude hiding (unsnoc)
import Data.Text (unsnoc, snoc, pack, unpack, append)
import Data.Metrology.Show
import Data.Metrology.Poly ( quOf )
import Data.Metrology.Vector ( MkQu_D, Dimension )
import Text.Parsec (parse)
import System.IO (hSetEcho, hReady, hGetChar, hPutChar, hSetBuffering, BufferMode (NoBuffering) )
import System.Console.ANSI

import Nummy.Parser (quantityT)

showTuple (n, u) = show n ++ " " ++ show u
action = parse quantityT ""
showAction s = either show showTuple $ action s

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  repl $ pack ""


repl :: Text -> IO ()
repl s = do
  c <- getKey
  new <- transform s c
  echoLine new
  -- print (c, new)
  repl new

transform :: Text -> [Char] -> IO Text
transform s c = case c of
    "\n" -> do
      hPutChar stdout '\n'
      putStrLn $ showAction (unpack s)
      return $ pack ""
    "\DEL" -> case unsnoc s of
                  Just (t, _) -> return t
                  Nothing -> return s
    _ -> return $ append s $ pack c

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where getKey' chars = do
          char <- hGetChar stdin
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)



echoLine :: Text -> IO ()
echoLine s = do
  -- hideCursor
  clearLine
  setCursorColumn 0
  putStr s

  saveCursor

  Just (_, col) <- getCursorPosition
  Just (w, _) <- getTerminalSize
  let peek = case action (unpack s) of
                Right t -> showTuple t
                Left _ -> ""
  let res = "= " ++ peek
  let maxl = min (w - col) (length res)
  setCursorColumn (w - maxl)
  putStr (take maxl res)

  restoreCursor
  showCursor
