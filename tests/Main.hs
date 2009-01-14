-- File created: 2009-01-06 12:56:34

module Main (main) where

import System.Environment (getArgs)
import Test.Framework

import qualified Tests.Properties as Properties
import qualified Tests.Strictness as Strictness

main = do
   args <- getArgs
   defaultMainWithArgs tests . concat $
      [ ["--timeout", show 10]
      , ["--maximum-generated-tests", show 200]
      , args
      ]

tests = concat
   [ Properties.tests
   , Strictness.tests
   ]
