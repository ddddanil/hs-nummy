module Main.Repl (repl) where

import Protolude
import Data.Text as T
import Data.Bifoldable ( bifold )
import System.IO (hSetEcho, hReady, hGetChar, hPutChar, hSetBuffering, BufferMode (NoBuffering) )
import System.Console.ANSI
import Text.Parsec (ParseError)

import Nummy.Metrology.Unit

type ReplAction = [Char] -> Either ParseError Quantity
data ReplState = ReplState { line :: Text, action :: ReplAction } -- deriving (Eq, Show)
type ReplStateM a = StateT ReplState IO a

repl :: ReplAction -> IO ()
repl action = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  setTitle "Nummy"
  evalStateT (forever readRepl) $ ReplState "" action

readRepl :: ReplStateM ()
readRepl = do
  st <- get
  echoLine
  -- echoLine $ show (c, line st)
  lift getKey >>= transform
  where
    pretty res =
        let peek = case res of
                      Right t -> t
                      Left _ -> ""
        in append "= " peek

transform :: [Char] -> ReplStateM ()
transform c = do
  Just (_, col) <- lift getCursorPosition
  let applyAtCursor f = (bifold . first f . T.splitAt col)
  let backspace s = case T.unsnoc s of Just(rest, _) -> rest; Nothing -> s
  case c of
    "\n" -> do
      lift $ hPutChar stdout '\n'
      lift . print =<< gets (\st -> action st $ unpack $ line st)
      modify $ \st -> st { line = "" }
    "\ESC[C" -> lift $ cursorForward 1
    "\ESC[D" -> lift $ cursorBackward 1
    ('\ESC':rest) -> return ()
    "\DEL" -> do
      modify $ \st -> st { line = applyAtCursor backspace (line st) }
      lift $ cursorBackward 1
    _ -> do
      modify $ \st -> st { line = applyAtCursor (`append` pack c) (line st) }
      lift $ cursorForward 1

getKey :: IO [Char]
getKey = Protolude.reverse <$> getKey' ""
  where getKey' chars = do
          char <- hGetChar stdin
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)

echoLine :: ReplStateM ()
echoLine = do
  st <- get
  let q = action st (unpack $ line st)
  Just (_, w) <- lift getTerminalSize

  lift $ do
    saveCursor
    clearLine
    setCursorColumn 0
    putStr $ line st
    setCursorColumn 140
    when (isRight q) $ let Right qu = q in putStr qu
    restoreCursor
