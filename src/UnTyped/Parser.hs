{-# LANGUAGE RecursiveDo #-}
module UnTyped.Parser where

import Prelude hiding (takeWhile)

import Text.Earley
import qualified Data.Text as T
import Data.Char
import Control.Applicative

import Data.Set hiding (foldr')
import qualified Data.List as L

import UnTyped.Syntax

termParser = mdo
  abstrac  <- rule $ (symbol "\\" *> (L.foldr ((.) . Abstrac) id <$> some name)) <*> (symbol "." *> apply)
  apply    <- rule $ L.foldl1' Apply <$> some atom <|> abstrac
  atom     <- rule $ constant <|> var <|> (symbol "(" *> apply <* symbol ")")
  return apply
  where constant = int <|> bool
        var  = Var <$> name
        name = T.pack <$> satisfy (all (\c -> 'a' <= c && c <= 'z'))
        int  = (Constant . CInt  . read) <$> satisfy (all isNumber)
        bool = Constant (CBool False) <$ satisfy ("False" ==) <|> Constant (CBool True) <$ satisfy ("True" ==)


tokenize :: String -> [String]
tokenize "" = []
tokenize (' ':rest)  = tokenize rest
tokenize total@(c:rest) = case member c reserved of
  True  -> [c] : tokenize rest
  False -> iden : tokenize other
  where reserved = fromList [' ','\\','.','(',')']
        (iden,other) = break (`member` reserved) total

allParse :: String -> ([Term],Report String [String])
allParse string = fullParses (parser termParser (tokenize string))

parse :: String -> Either String Term
parse string = case allParse string of
  (x:_,_) -> Right x
  (_,err) -> Left (concat (expected err))
