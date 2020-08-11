module Application.Repl (
  ReplAction
, repl
, rep
) where

import Nummy.Prelude
import Pipes
import Pipes.Concurrent
import System.IO (hSetEcho, hSetBuffering, BufferMode (NoBuffering))
import System.Console.ANSI

import Nummy.Parser
import Application.Repl.Input
import Application.Repl.Output


type ReplAction = Text -> IO ParserResult

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
  i <- async . runEffect $ runInput >-> toOutput ( o1 <> o2 )
  -- Parser
  p <- async . runEffect $ fromInput i2 >-> runParser pa >-> toOutput o1
  -- Output
  o <- async . runEffect $ fromInput i1 >-> runOutput
  -- run all
  mapM_ wait [i, p, o]

rep :: Text -> ReplAction -> IO ()
rep s pa = do
  -- Prep
  setTitle "Nummy"
  -- Pipes
  let e1 = OPrompt s 1                                -- fake input
  Right (e2, _) <- next $ yield e1 >-> runParser pa   -- run parser over input
  let e3 = OCommand                                   -- fake Return press
  runEffect $ each [ e1, e2, e3 ] >-> runOutput       -- Send three events in sequence

runParser :: ReplAction -> Pipe OutputEvent OutputEvent IO ()
runParser p =
  forever $ do
    e <- await
    case e of
      OPrompt s _ -> do
        res <- liftIO $ p s
        yield $ OResult res
      _ -> return ()

