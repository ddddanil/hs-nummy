module Main.Repl (repl) where

import Protolude
-- import Control.Monad.Trans (lift)
-- import Control.Monad.Trans.State.Strict
import Data.Text as T
import Data.Bifoldable ( bifold )
import System.IO (hSetEcho, hReady, hGetChar, hPutChar, hSetBuffering, BufferMode (NoBuffering) )
import System.Console.ANSI

data ReplState = ReplState { line :: Text, cursor :: Int } deriving (Eq, Show)
type ReplStateM a = StateT ReplState IO a
type ReplAction e s = s -> Either e s

repl :: Show e => ReplAction e Text -> IO ()
repl action = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  setTitle "Nummy"
  evalStateT (forever $ readRepl action) $ ReplState "" 0

readRepl :: Show e => ReplAction e Text -> ReplStateM ()
readRepl a = do
  c <- lift $ getKey
  transform c
  st <- get
  -- echoLine $ pretty (a (line st))
  echoLine $ show (c, (line st))

pretty res =
    let peek = case res of
                  Right t -> t
                  Left _ -> ""
    in append "= " peek

transform :: [Char] -> ReplStateM ()
transform c = do
  Just (_, col) <- lift getCursorPosition
  let applyBackspace s = case T.unsnoc s of Just(rest, _) -> rest; Nothing -> s
  let moveCursorLeft = modify $ \st -> st { cursor = cursor st - 1 }
  let moveCursorRight = modify $ \st -> st { cursor = cursor st + 1 }
  case c of
    "\n" -> do
      lift $ hPutChar stdout '\n'
      modify $ \st -> st { line = "" }
    "\ESC[C" -> moveCursorRight
    "\ESC[D" -> moveCursorLeft
    ('\ESC':rest) -> return ()
    "\DEL" -> do
      modify $ \st -> st { line = (bifold . first applyBackspace . T.splitAt col) (line st) }
      moveCursorLeft
    _ -> do
      modify $ \st -> st { line = (bifold . first ((flip append) (pack c)) . T.splitAt col) (line st) }
      moveCursorRight

getKey :: IO [Char]
getKey = Protolude.reverse <$> getKey' ""
  where getKey' chars = do
          char <- hGetChar stdin
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)



echoLine :: Text -> ReplStateM ()
echoLine s = do
  st <- get
  lift $ do
    clearLine
    setCursorColumn 0
    putStr $ line st

    Just (w, _) <- getTerminalSize
    let maxl = min (w - (T.length (line st))) (T.length s)
    setCursorColumn (w - maxl)
    putStr (T.take maxl s)

    setCursorColumn $ cursor st
