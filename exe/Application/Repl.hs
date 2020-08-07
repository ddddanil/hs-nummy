module Application.Repl (
  ReplAction
, repl
) where

import Nummy.Prelude
import Pipes
import qualified Pipes.Prelude as P
import Pipes.Concurrent
import Control.Concurrent.Async
import Data.Functor.Contravariant (contramap)
import System.IO (putChar, hReady, hSetEcho, hSetBuffering, BufferMode (NoBuffering))
import System.Console.ANSI

import Application.Repl.Input
import Application.Repl.Output


type ReplAction = Text -> IO Text

repl :: ReplAction -> IO ()
repl p = do
  -- Prep
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  setTitle "Nummy"
  -- Setup boxes
  (o1, i1, s1) <- spawn' Unbounded -- to output
  (o2, i2, s2) <- spawn' Unbounded -- to parser
  -- Input
  i <- async . runEffect $ runInput >-> toOutput ( ( contramap (uncurry OPrompt) o1 ) <> ( contramap fst o2 ) )
  -- Parser
  p <- async . runEffect $ fromInput i2 >-> runParser p >-> toOutput o1
  -- Output
  o <- async . runEffect $ fromInput i1 >-> runOutput
  -- run all
  mapM_ wait [i, p, o]


runParser :: ReplAction -> Pipe Text OutputEvent IO ()
runParser p = P.mapM (fmap OResult <$> p)
