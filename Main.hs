{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}

module Main where

import Polysemy
import qualified System.Eval.Haskell as Haskell

import Effects

handleFailures :: Either [String] (Maybe (Sem [TeletypePlus, Teletype, Embed IO] ())) -> IO Sem [TeletypePlus, Teletype, Embed IO] ()
handleFailures (Left ss) = ioError $ userError $ concat ss
handleFailures (Right Nothing) = ioError $ userError "eval_ returned Right Nothing."
handleFailures (Right (Just s)) = return s

echo :: IO (Sem [TeletypePlus, Teletype, Embed IO] ())
echo = do
  userProvided <- readFile "UserProvided.hs"
  raw <- fmap handleFailures $
    Haskell.eval_
      userProvided
      ["Effects"] --include
      [] --make flags
      [] --package.confs
      [] --include paths
  return raw
    
-- echo forever
main :: IO ()
main = runM . teletypeToIO . teletypePlusToIO $ echo

