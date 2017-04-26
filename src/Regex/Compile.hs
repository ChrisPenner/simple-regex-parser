module Regex.Compile
  ( match
  ) where

import Regex.Parse
-- import Data.Foldable
import Control.Monad.State

-- import Text.Parsec.Error

data RState = RState
  { current :: String
  , groups :: Groups
  , leftover :: String
  }

type Leftover = String
type Current = String
type Match = (Groups, Current, String)
-- type Pattern = String
type Groups = [String]

-- match :: Pattern -> String -> Either ParseError Match
-- match pattern str = flip match' str <$> parseRegex pattern



match :: Expr -> Groups -> String -> [Match]
match (Atom c) groups (x:xs)
  | c == x = [(groups, [x], xs)]
  | otherwise = []
match Wildcard groups (x:xs) = [(groups, [x], xs)]
match (OneOf terms) groups str = do
  term <- terms
  match term groups str

match (Group next) groups str = fmap addGroup $ match next groups str
  where addGroup (grps, cur, leftover) = (grps ++ [cur], cur, leftover)

match (Terms terms) groups str = foldl go [(groups, "", str)] terms
  where go matches term = do
            (grps, cur, leftover) <- matches
            (grps', cur', leftover') <- match term grps leftover
            return (grps', cur++cur', leftover')
match _ _ [] = []
