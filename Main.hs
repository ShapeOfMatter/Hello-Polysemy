module Main where

import Data.List (intercalate)
import Polysemy (runM)
import qualified Language.Haskell.Interpreter as H

import qualified Effects as E

handleFailures :: Either H.InterpreterError a -> IO a
handleFailures (Left l) = ioError $ userError $ message l
  where
    message (H.WontCompile es) = intercalate "\n" (header : map unbox es)
    message e = show e
    header = "ERROR: Won't compile:"
    unbox (H.GhcError e) = e
handleFailures (Right a) = return a

interpretation :: String -> H.Interpreter E.MyEffect
interpretation s = do
  H.loadModules ["Effects"]
  H.setImportsQ [("Prelude", Nothing), ("Effects", Nothing)]
  effect <- H.interpret s (H.as :: E.MyEffect)
  return effect

extractProgram :: String -> IO E.MyEffect
extractProgram s = do
  p <- H.runInterpreter $ interpretation s
  success <- handleFailures p
  return success

main :: IO ()
main = do
  userProvided <- readFile "UserProvided2.hs"
  print userProvided
  userProgram <- extractProgram userProvided
  runM . E.teletypeToIO . E.teletypePlusToIO $ userProgram

