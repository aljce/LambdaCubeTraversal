module SimplyTyped.Main where

import Data.Bifunctor

import System.Console.Haskeline

import SimplyTyped.Syntax
import SimplyTyped.Parser
import SimplyTyped.Check
import SimplyTyped.Eval

parseAndCheck uinput = do
  term <- first ("Parse error: "++) (parse uinput)
  first show (runCheck term)
  return (fullEval term)

handleInput term = case parseAndCheck term of
  Left err -> outputStrLn err
  Right reduced -> mapM_ (outputStrLn . show) reduced

main :: IO ()
main = runInputT defaultSettings loop
  where loop = do
          input <- getInputLine "simple> "
          case input of
            Nothing     -> return ()
            Just ""     -> goodbye
            Just "q"    -> goodbye
            Just "quit" -> goodbye
            Just uinput -> handleInput uinput >> loop
        goodbye = outputStrLn "Goodbye."
