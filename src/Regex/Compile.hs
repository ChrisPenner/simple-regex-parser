{-# language TemplateHaskell #-}
{-# language GeneralizedNewtypeDeriving #-}
module Regex.Compile where

import Regex.Parse
import Control.Monad.State
import List.Transformer as LT
import Data.List
import Data.Foldable (asum)
import Control.Lens
import Data.Monoid

-- import Text.Parsec.Error

type Groups = [String]
type Pattern = String

data RState = RState
  { _groups :: Groups
  , _leftover :: String
  } deriving Show

makeLenses ''RState

-- compile :: Pattern -> String -> Either ParseError (Maybe String)
-- compile pat str = fst . flip getMatch str <$> parseRegex pat

-- getMatch :: Expr -> String -> [(String, RState)]
-- getMatch expr str = flip evalState (RState [] str) $ do
--   result <- next lst
--   case result of
--     Nil -> return []
--     (Cons x rest) -> do
--       st <- get
--       ((x, st):) <$> next rest
--     where lst = match expr

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

  -- (RState [] str) $ do
  -- result <- next lst
  -- return $ case result of
  --            (Cons x _) -> Just x
  --            _ -> Nothing
  --   where lst = match expr




-- getMatches :: Expr -> String -> (Maybe String)
-- getMatches expr str = flip runState (RState [] str) $ do
--   result <- next lst
--   return $ case result of
--              (Cons x _) -> Just x
--              _ -> Nothing
--     where lst = match expr




-- getMatches :: Expr -> String -> ([String], RState)
-- getMatches expr str = flip runState (RState [] str) $ do
--   result <- next lst
--   recurse result
--     where
--       lst = match expr
--       recurse (Cons x xs) = do
--         ys <- next xs >>= recurse
--         return (x:ys)
--       recurse Nil = return []
-- getMatch (Repetition (Atom 'a') 10 Nothing) "aaaaaa"

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

match (Group term) = do
  theMatch <- match term
  groups <>= [theMatch]
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
match (Repetition _ 0 (Just 0)) = pure ""
match (Repetition term 0 mEnd) = go
  where
    go = do
      m <- match term
      grps <- use groups
      st <- get
      results <- (mappend m <$> match (Repetition term 0 (subtract 1 <$> mEnd))) <|> (put st >> pure m)
      groups .= grps
      return results

match (Repetition term start mEnd) = do
  m <- match term
  grps <- use groups
  recursed <- match (Repetition term (start - 1) (subtract 1 <$> mEnd))
  groups .= grps
  return $ m <> recursed


match _ = empty
