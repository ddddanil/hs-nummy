{-# LANGUAGE TemplateHaskell #-}
module Application.Repl.Output (
  OutputEvent (..)
, runOutput
) where

import Nummy.Prelude
import Pipes
import Control.Lens
import Text.Megaparsec
import qualified Data.Text as T
import System.IO (putChar, hFlush)
import System.Console.ANSI

import Nummy.Parser

-- Types

data OutputEvent
  = OPrompt Text Int
  | OResult ParserResult
  | OCommand
  deriving (Eq, Show)
makePrisms ''OutputEvent

data OutputState = OutputState
  { _prompt :: Text
  , _cursor :: Int
  , _result :: ParserResult
  }
makeLenses ''OutputState

type OutputM = StateT OutputState (Consumer OutputEvent IO)


-- Pipes

runOutput :: Consumer OutputEvent IO ()
runOutput = evalStateT (printOutput >> forever (updateState >> printOutput) ) startState


-- Stateful functions

startState :: OutputState
startState = OutputState "" 0 (PResult "")

updateState :: OutputM ()
updateState = do
  e <- lift await
  case e of
    OPrompt i c -> do
      prompt .= i
      cursor .= c
    OResult o   -> do
      result .= o
    OCommand -> do
      printExec


-- Output

printOutput :: OutputM ()
printOutput = do
  let pr = "> " :: Text
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
    putStr . printResult $ r
    setCursorColumn (c + T.length pr)
    hFlush stdout

printResult :: ParserResult -> Text
printResult r =
  case r of
    PError _ -> " ✗"
    PResult x -> T.append " = " x

printExec :: OutputM ()
printExec = do
  r <- use result
  liftIO $ do
    putChar '\n'
    setCursorColumn 0
    case r of
      PResult _ -> return ()
      PError e -> putStrLn $ errorBundlePretty e
