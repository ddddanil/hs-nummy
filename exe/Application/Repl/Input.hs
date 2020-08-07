{-# LANGUAGE TemplateHaskell #-}
module Application.Repl.Input (
  runInput
) where

import Nummy.Prelude
import Pipes
import Control.Lens
import Data.Char (isPrint)
import Data.Bifoldable (bifoldl1)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import System.IO (getChar, hReady)


-- Types

type InputM = StateT InputState (Producer (Text, Int) IO)

data InputState = InputState
  { _input :: (Text, Int)        -- ^ Text and cursor position inside it
  }
makeLenses ''InputState


-- Pipes

runInput :: Producer (Text, Int) IO ()
runInput = evalStateT (forever handleInput) startState

startState :: InputState
startState = InputState ("", 0)


-- Stateful functions

pushState :: InputM ()
pushState = lift . yield =<< use input

moveCursorRight :: InputM ()
moveCursorRight = input %= advRight where
  advRight (str, pos) = (str, min (T.length str) (pos + 1) )

moveCursorLeft :: InputM ()
moveCursorLeft = input %= advLeft where
  advLeft (str, pos) = (str, max 0 (pos - 1) )

insertChar :: Text -> Int -> Char -> Text
insertChar s p c = bifoldl1 (T.append) $ second (T.cons c) (T.splitAt p s)

removeChar :: Text -> Int -> Text
removeChar s p = T.append (T.take p s) (T.drop (p+1) s)

addCharLeft :: Char -> InputM ()
addCharLeft c = do
  (str, pos) <- use input
  input . _1 .= insertChar str pos c
  moveCursorRight

remCharLeft :: InputM ()
remCharLeft = do
  (str, pos) <- use input
  input . _1 .= removeChar str (pos - 1)
  moveCursorLeft

addCharRight :: Char -> InputM ()
addCharRight c = do
  (str, pos) <- use input
  input . _1 .= insertChar str pos c

remCharRight :: InputM ()
remCharRight = do
  (str, pos) <- use input
  input . _1 .= removeChar str (pos - 1)


-- Input

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

handleInput :: InputM ()
handleInput = do
  i <- liftIO getKey
  whenJust (printableSequence i) addCharLeft
  whenJust (M.lookup i key_table) identity
  pushState

key_table :: M.Map [Char] (InputM ())
key_table = M.fromList $
  [ ("\ESC[C", moveCursorRight)
  , ("\ESC[D", moveCursorLeft )
  , ("\DEL",   remCharLeft    )
  ]


