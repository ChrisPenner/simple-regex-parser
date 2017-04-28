module Regex.Parse
  ( parseRegex
  , Expr(..)
  ) where

import Text.Read (readMaybe)
import Text.Parsec hiding (State, Empty)
import Control.Monad.State

type Parser = ParsecT String () (State Int) Expr

data Expr
  = Group Int Expr
  | Terms [Expr]
  | OneOf [Expr]
  | Repetition Expr Int (Maybe Int)
  | Empty
  | BackRef Int
  | Wildcard
  | Atom Char
  | Start
  | End
  deriving (Show)

specialChars :: String
specialChars = "\\[]|{}().+*?"

parseRegex :: String -> Either ParseError Expr
parseRegex = flip evalState 1 . runParserT regex () "Regex"

regex :: Parser
regex = expr <* eof

expr :: Parser
expr = OneOf <$> terms `sepBy1` char '|'

terms :: Parser
terms = Terms <$> many1 term

term :: Parser
term = do
  re <- choice [group, range, token']
  option re (quantifier re <|> repetition re)

quantifier :: Expr -> Parser
quantifier re = do
  c <- oneOf "?*+"
  return $ case c of
             '?' -> OneOf [Empty, re]
             '*' -> OneOf [Empty, Repetition re 1 Nothing]
             '+' -> Repetition re 1 Nothing
             _ -> error "Impossible pattern match fail"

repetition :: Expr -> Parser
repetition re =
  between (char '{') (char '}') $ do
    start <- read <$> many1 digit
    end <- option Nothing $ do
      _ <- char ','
      spaces
      readMaybe <$> many digit
    return $ if start == 0
                  then OneOf [Empty, Repetition re start end]
                  else Repetition re start end

group :: Parser
group = do
  i <- get
  modify (+1)
  res <- between (char '(') (char ')') expr
  return $ Group i res

token' :: Parser
token' = start <|> end <|> wildcard <|> atom <|> try escaped <|> backRef
  where
    start = char '^' *> return Start
    end = char '$' *> return End
    wildcard = char '.' *> return Wildcard
    atom = Atom <$> noneOf specialChars
    escaped = Atom <$> (char '\\' *> oneOf specialChars)
    backRef = BackRef . read <$> (char '\\' *> many1 digit)

range :: Parser
range = OneOf . fmap Atom <$> possibleChars
  where ranges = concat <$> many1 (try span' <|> lit)
        possibleChars = between (char '[') (char ']') ranges
        lit = (:[]) <$> noneOf "]"
        span' = do
          start <- noneOf "]"
          _ <- char '-'
          end <- noneOf "]"
          return [start..end]
