{-# LANGUAGE TemplateHaskell #-}
module Application.Repl.Output (
  OutputEvent (..)
, runOutput
) where

import Nummy.Prelude
import Pipes
import qualified Pipes.Prelude as P
import Control.Lens
import qualified Data.Text as T
import System.IO (getChar, putChar, hFlush)
import System.Console.ANSI


-- Types

data OutputEvent
  = OPrompt Text Int
  | OResult Text
  deriving (Eq, Ord, Show)

data OutputState = OutputState
  { _prompt :: Text
  , _cursor :: Int
  , _result :: Text
  }
makeLenses ''OutputState

type OutputM = StateT OutputState (Consumer OutputEvent IO)


-- Pipes

runOutput :: Consumer OutputEvent IO ()
runOutput = evalStateT (printOutput >> forever (updateState >> printOutput) ) startState


-- Stateful functions

startState :: OutputState
startState = OutputState "" 0 ""

updateState :: OutputM ()
updateState = do
  e <- lift await
  case e of
    OPrompt i c -> do
      prompt .= i
      cursor .= c
    OResult o   -> do
      result .= o


-- Output

printOutput :: OutputM ()
printOutput = do
  let pr = "> " :: Text
  let trail = " = " :: Text
  p <- use prompt
  r <- use result
  c <- use cursor
  liftIO $ do
    clearLine
    setCursorColumn 0
    setSGR [Reset]
    putStr pr
    setSGR [ SetColor Foreground Vivid White ]
    putStr p
    setSGR [ SetColor Foreground Dull White ]
    putChar ' '
    putStr trail
    putStr r
    setCursorColumn (c + T.length pr)
    hFlush stdout
