module UnTyped.Main where

import System.Console.Haskeline

import UnTyped.Syntax
import UnTyped.Parser
import UnTyped.Eval

eval :: Term -> Either String [Term]
eval term = loop 0 term (stepEval term) [term]
  where loop :: Int -> Term -> Term -> [Term] -> Either String [Term]
        loop count oldterm newterm terms = case (count >= 100) of
          True  -> Left "<diverges>"
          False -> case oldterm == newterm of
            True  -> return (reverse terms)
            False -> loop (count + 1) newterm (stepEval newterm) (newterm : terms)

evalAndPrint :: String -> InputT IO ()
evalAndPrint input = case parse input >>= eval of
  Left err     -> outputStrLn ("Error: " ++ err)
  Right parsed -> mapM_ (outputStrLn . show) parsed

main :: IO ()
main = runInputT defaultSettings loop
  where loop = do
          input <- getInputLine "untyped> "
          case input of
            Nothing -> return ()
            Just "quit" -> outputStrLn "Goodbye."
            Just input  -> evalAndPrint input >> loop
