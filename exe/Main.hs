module Main where

import Nummy.Prelude
import Application.Arguments
import Application.Base


main :: IO ()
main = tryArgs custom_repl
