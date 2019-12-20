{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}

module Effects where

import Polysemy

data Teletype m a where
  ReadTTY  :: Teletype m String
  WriteTTY :: String -> Teletype m ()

makeSem ''Teletype

teletypeToIO :: Member (Embed IO) r => Sem (Teletype ': r) a -> Sem r a
teletypeToIO = interpret $ \case
  ReadTTY      -> embed getLine
  WriteTTY msg -> embed $ putStrLn msg

data TeletypePlus m a where
  ReadPlus  :: TeletypePlus m String
  WritePlus :: String -> TeletypePlus m ()

makeSem ''TeletypePlus

teletypePlusToIO :: Member (Embed IO) r => Sem (TeletypePlus ': r) a -> Sem r a
teletypePlusToIO = interpret $ \case
  ReadPlus      -> embed $ ("+" <>) <$> getLine
  WritePlus msg -> embed $ putStrLn $ msg <> "+"

type MyEffect = Sem [TeletypePlus, Teletype, Embed IO] ()
