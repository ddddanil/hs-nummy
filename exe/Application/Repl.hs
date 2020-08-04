{-# LANGUAGE TemplateHaskell #-}
module Application.Repl (
  ReplAction
, repl
) where

import Nummy.Prelude
import Control.Lens
import Control.Monad.Fail
import Control.Monad.Except (liftEither)
import Data.Char (isPrint)
import Data.Bifoldable (bifoldl1, bimsum)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.IO (getChar, putChar, hReady, hSetEcho, hSetBuffering, BufferMode (NoBuffering))
import System.Console.ANSI

type Repl = StateT ReplState IO

type ReplAction = Text -> IO Text

data ReplState = ReplState
  { _action :: ReplAction
  , _input :: (Text, Int) -- Text and cursor position inside it
  }
makeLenses ''ReplState

repl :: ReplAction -> IO ()
repl f = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  putChar '\n'
  setTitle "Nummy"
  runRepl (forever (updateRepl >> handleInput) ) f

traceSt :: Repl a -> Repl a
traceSt m = do
  x <- m
  st <- gets (^. input)
  liftIO . print $ st
  return x

runRepl :: Repl a -> ReplAction -> IO a
runRepl m f = do
  evalStateT m (startState f)

startState :: ReplAction -> ReplState
startState f = ReplState f ("", 0)

runAction :: Repl Text
runAction = do
  inp <- gets (^. input . _1)
  act <- gets (^. action)
  lift $ act inp

moveCursorRight :: Repl ()
moveCursorRight = input %= advRight where
  advRight (str, pos) = (str, min (T.length str) (pos + 1) )

moveCursorLeft :: Repl ()
moveCursorLeft = input %= advLeft where
  advLeft (str, pos) = (str, max 0 (pos - 1) )

insertChar :: Text -> Int -> Char -> Text
insertChar s p c = bifoldl1 (T.append) $ second (T.cons c) (T.splitAt p s)

removeChar :: Text -> Int -> Text
removeChar s p = T.append (T.take p s) (T.drop (p+1) s)

addCharLeft :: Char -> Repl ()
addCharLeft c = do
  (str, pos) <- gets (^. input)
  input . _1 .= insertChar str pos c
  moveCursorRight

remCharLeft :: Repl ()
remCharLeft = do
  (str, pos) <- gets (^. input)
  input . _1 .= removeChar str (pos - 1)
  moveCursorLeft

addCharRight :: Char -> Repl ()
addCharRight c = do
  (str, pos) <- gets (^. input)
  input . _1 .= insertChar str pos c

remCharRight :: Repl ()
remCharRight = do
  (str, pos) <- gets (^. input)
  input . _1 .= removeChar str (pos - 1)

updateRepl :: Repl ()
updateRepl = do
  (i, pos) <- gets (^. input)
  o <- runAction `catchError` (\_ -> return "No parse")
  -- o <- gets (show . (^. input))
  liftIO $ do
    offset <- replPrint i o
    setCursorColumn (offset + pos)

replPrint :: Text -> Text -> IO Int
replPrint i o = do
  let prompt = "> " :: Text
  let leader = "= " :: Text
  setCursorColumn 0
  clearLine
  putStr prompt
  putStr i
  cursorDown 1
  setCursorColumn 0
  clearLine
  putStr leader
  putStr o
  cursorUp 1
  setCursorColumn 0
  return $ T.length prompt

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where getKey' chars = do
          char <- getChar
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)

printableSequence :: [Char] -> Maybe Char
printableSequence s = do
  c <- headMay s
  guard $ length s == 1
  guard $ isPrint c
  return c

handleInput :: Repl ()
handleInput = do
  i <- liftIO getKey
  whenJust (printableSequence i) addCharLeft
  whenJust (M.lookup i key_table) identity


key_table :: M.Map [Char] (Repl ())
key_table = M.fromList $
  [ ("\ESC[C", moveCursorRight)
  , ("\ESC[D", moveCursorLeft )
  , ("\DEL",   remCharLeft    )
  ]
