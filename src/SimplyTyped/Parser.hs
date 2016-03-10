{-# LANGUAGE RecursiveDo #-}
module SimplyTyped.Parser where

import Text.Earley
import Control.Applicative

import qualified Data.List as L
import Data.Set

import SimplyTyped.Syntax

sepBy1 p op = mdo
  ops <- rule $ pure [] <|> (:) <$ op <*> p <*> ops
  rule $ (:) <$> p <*> ops

typeParser = mdo
  arrow <- fmap (foldr1 TArr) <$> atom `sepBy1` symbol "->"
  atom  <- rule $ unit <|> (symbol "(" *> arrow <* symbol ")")
  return arrow
  where unit :: Prod r String String Type
        unit = T <$ satisfy (all (\c -> 'a' <= c && c <= 'z'))

termParser = mdo
  types    <- typeParser
  abstrac  <- rule $ (symbol "\\" *> (Abstrac <$> name)) <*>
    (symbol ":" *> types) <*> (symbol "." *> apply)
  apply    <- rule $ L.foldl1' Apply <$> some atom <|> abstrac
  atom     <- rule $ var <|> (symbol "(" *> apply <* symbol ")")
  return apply
  where var  = Var <$> name
        name = satisfy (all (\c -> 'a' <= c && c <= 'z'))

tokenize :: String -> [String]
tokenize "" = []
tokenize (' ':rest)  = tokenize rest
tokenize total@(c:rest) = case member c reserved of
  True  -> [c] : tokenize rest
  False -> iden : tokenize other
  where reserved = fromList [' ','\\','.','(',')',':']
        (iden,other) = break (`member` reserved) total

allParse :: String -> ([Term],Report String [String])
allParse string = fullParses (parser termParser (tokenize string))

parse :: String -> Either String Term
parse string = case allParse string of
  (x:_,_) -> Right x
  (_,err) -> Left (concat (expected err))

