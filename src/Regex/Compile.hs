{-# language TemplateHaskell #-}
{-# language GeneralizedNewtypeDeriving #-}
module Regex.Compile where

import Regex.Parse
import Control.Monad.State
import List.Transformer as LT
import Data.List
import Data.Foldable (asum)
import Control.Lens hiding (Empty)
import Data.Monoid
import qualified Data.IntMap as IM

import Text.Megaparsec.Error

type Groups = IM.IntMap String
type Pattern = String

data RState = RState
  { _groups :: Groups
  , _leftover :: String
  } deriving Show

makeLenses ''RState

compile :: Pattern -> String -> Either (ParseError Char Dec) [(String, RState)]
compile pat str = flip evalState (RState IM.empty str) . getMatches <$> parseRegex pat

getMatches :: Expr -> State RState [(String, RState)]
getMatches expr = LT.foldM go def toM $ match expr
  where
    toM :: [(String, RState)] -> State RState [(String, RState)]
    toM = return
    def :: State RState [(String, RState)]
    def = return []
    go :: [(String, RState)] -> String -> State RState [(String, RState)]
    go acc nxt = do
      st <- get
      return $ if null $ _leftover st
                  then (nxt, st):acc
                  else acc

match :: Expr -> ListT (State RState) String
match (Atom c) = do
  (x:xs) <- use leftover
  if x == c
     then leftover .= xs >> return [x]
     else empty

match Wildcard  = do
  (x:xs) <- use leftover
  leftover .= xs
  return [x]

match (OneOf terms) = do
  st <- get
  -- Backtrack the state and try again on each alternative
  let try t = put st >> match t
  asum (try <$> terms)

match (Group n term) = do
  theMatch <- match term
  groups . at n ?= theMatch
  return theMatch

match (Terms terms) =
  foldl go (return "") terms
  where
    go acc term = do
      a <- acc
      b <- match term
      return (a <> b)

match (BackRef n) = do
  mGroup <- preuse (groups . ix n)
  str <- use leftover
  case extractMatch mGroup str of
    Nothing -> empty
    Just (matched, rest) -> do
      leftover .= rest
      return matched
  where
    extractMatch mGroup str = do
      grp <- mGroup
      rest' <- stripPrefix grp str
      return (grp, rest')

-- Match with longest sequences first; i.e. be greedy
match (Repetition _ _ (Just 0)) = empty
match (Repetition term 1 mEnd) = go
  where
    go = do
      m <- match term
      st <- get
      (mappend m <$> match (Repetition term 1 (subtract 1 <$> mEnd)))
        <|> (put st >> pure m)

match (Repetition term start mEnd) = do
  m <- match term
  mappend m <$> match (Repetition term (start - 1) (subtract 1 <$> mEnd))

match Empty = pure ""

match _ = empty
