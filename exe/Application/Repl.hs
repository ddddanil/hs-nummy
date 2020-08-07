module Application.Repl (
  ReplAction
, repl
) where

import Nummy.Prelude
import Pipes
import qualified Pipes.Prelude as P
import Pipes.Concurrent
import Data.Functor.Contravariant (contramap)
import System.IO (hSetEcho, hSetBuffering, BufferMode (NoBuffering))
import System.Console.ANSI

import Application.Repl.Input
import Application.Repl.Output


type ReplAction = Text -> IO Text

repl :: ReplAction -> IO ()
repl pa = do
  -- Prep
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  setTitle "Nummy"
  -- Setup boxes
  (o1, i1) <- spawn unbounded -- to output
  (o2, i2) <- spawn unbounded -- to parser
  -- Input
  i <- async . runEffect $ runInput >-> toOutput ( ( contramap (uncurry OPrompt) o1 ) <> ( contramap fst o2 ) )
  -- Parser
  p <- async . runEffect $ fromInput i2 >-> runParser pa >-> toOutput o1
  -- Output
  o <- async . runEffect $ fromInput i1 >-> runOutput
  -- run all
  mapM_ wait [i, p, o]


runParser :: ReplAction -> Pipe Text OutputEvent IO ()
runParser p = P.mapM (fmap OResult <$> p)
