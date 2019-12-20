{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}

module Main where

import Data.List (intercalate)
import Polysemy
import qualified Language.Haskell.Interpreter as H

import Effects

handleFailures :: Either H.InterpreterError a -> IO a
handleFailures (Left l) = ioError $ userError $ message l
  where
    message (H.WontCompile es) = intercalate "\n" (header : map unbox es)
    message e = show e
    header = "ERROR: Won't compile:"
    unbox (H.GhcError e) = e
handleFailures (Right a) = return a

interpretation :: String -> H.Interpreter MyEffect
interpretation s = do
  H.setImportsQ [("Prelude", Nothing), ("Effects", Nothing)]
  effect <- H.interpret s (H.as :: MyEffect)
  return effect

extractProgram :: String -> IO MyEffect
extractProgram s = do
  p <- H.runInterpreter $ interpretation s
  success <- handleFailures p
  return success

main :: IO ()
main = do
  userProvided <- readFile "UserProvided.hs"
  userProgram <- extractProgram userProvided
  runM . teletypeToIO . teletypePlusToIO $ userProgram

