{-# language TemplateHaskell #-}
{-# language GeneralizedNewtypeDeriving #-}
module Regex.Compile where

import Regex.Parse
-- import Control.Monad.State
import List.Transformer
import Control.Lens
import Data.Monoid


type Groups = [String]
type Leftover = String
type Current = String

data RState = RState
  { _current :: String
  , _groups :: Groups
  , _leftover :: String
  } deriving Show

makeLenses ''RState

-- newtype RCompile m a =
--   RCompile (ListT m a)
--   deriving (Functor, Applicative, Monad)

getMatch :: ListT Identity RState -> Maybe RState
getMatch (ListT (Identity (Cons x _))) = Just x
getMatch _ = Nothing

getMatches :: ListT Identity RState -> [RState]
getMatches = filter (null . _leftover) . getMatches'

getMatches' :: ListT Identity RState -> [RState]
getMatches' = runIdentity . fold (\acc x -> acc ++ [x]) [] id

match :: Expr -> RState -> ListT Identity RState
match (Atom c) (RState cur grps (x:xs)) | c == x =
  return (RState (cur <> [c]) grps xs)

match Wildcard (RState cur grps (x:xs)) = return (RState (cur <> [x]) grps xs)
match (OneOf terms) rstate = select terms >>= flip match rstate
match (Group nxt) rstate = addGroup <$> match nxt rstate
  where addGroup (RState cur grps lft) = RState cur (grps ++ [cur]) lft

-- match (Terms terms) groups str = foldl go [(groups, "", str)] terms
--   where go matches term = do
--             (grps, cur, leftover) <- matches
--             (grps', cur', leftover') <- match term grps leftover
--             return (grps', cur++cur', leftover')
-- match _ _ [] = []
match _ _ = empty
