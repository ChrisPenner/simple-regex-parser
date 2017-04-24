module Regex.Compile
  ( RegexPattern(..)
  ) where

-- import Regex.Parse

data RegexPattern
  = ManyOf RegexPattern
  | Group RegexPattern
  | Singleton RegexPattern
  | Optional RegexPattern

-- compile :: [RegexToken] -> RegexPattern
-- compile = undefined
