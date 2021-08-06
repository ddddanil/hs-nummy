module Application.Repl (
  repl
, rep
) where

import Nummy.Prelude hiding (yield)
import Pipes
import Pipes.Concurrent
import System.IO (hSetEcho, hSetBuffering, BufferMode (NoBuffering))
import System.Console.ANSI

import Nummy.Cache
import Application.Repl.Input
import Application.Repl.Output
import Application.Repl.Parser


repl :: IO ()
repl = do
  -- Prep
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  setTitle "Nummy"
  -- Setup boxes
  (o1, i1) <- spawn unbounded -- to output
  (o2, i2) <- spawn unbounded -- to parser
  -- Input
  i <- async . runEffect $ runInput >-> toOutput ( o1 <> o2 )
  -- Parser
  p <- async . runReadCache . runEffect $ fromInput i2 >-> runParser >-> toOutput o1
  -- Output
  o <- async . runEffect $ fromInput i1 >-> runOutput
  -- run all
  mapM_ wait [i, p, o]

rep :: Text -> IO ()
rep s = do
  -- Prep
  setTitle "Nummy"
  -- Pipes
  let e1 = OPrompt s 1                                -- fake input
  Right (e2, _) <- runReadCache . next
                    $ yield e1 >-> runParser          -- run parser over input
  let e3 = OCommand                                   -- fake Return press
  runEffect $ each [ e1, e2, e3 ] >-> runOutput       -- Send three events in sequence

