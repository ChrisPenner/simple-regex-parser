module Regex.Parse
  ( parseRegex
  , Expr(..)
  ) where

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Lexer (integer)
import Control.Monad.State

type Parser = ParsecT Dec String (State Int)

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

int :: Parser Int
int = fromInteger <$> integer

specialChars :: String
specialChars = "\\[]|{}().+*?"

parseRegex :: String -> Either (ParseError Char Dec) Expr
parseRegex = flip evalState 1 . runParserT regex "Regex"

regex :: Parser Expr
regex = expr <* eof

expr :: Parser Expr
expr = OneOf <$> terms `sepBy1` char '|'

terms :: Parser Expr
terms = Terms <$> some term

term :: Parser Expr
term = do
  re <- choice [group, range, token']
  option re (quantifier re <|> repetition re)

quantifier :: Expr -> Parser Expr
quantifier re = do
  c <- oneOf "?*+"
  return $ case c of
             '?' -> OneOf [Empty, re]
             '*' -> OneOf [Empty, Repetition re 1 Nothing]
             '+' -> Repetition re 1 Nothing
             _ -> error "Impossible pattern match fail"

repetition :: Expr -> Parser Expr
repetition re =
  between (char '{') (char '}') $ do
    start <- int
    mEnd <- option Nothing $ do
      _ <- char ','
      space
      optional int
    return $ if start == 0
                  then OneOf [Empty, Repetition re start mEnd]
                  else Repetition re start mEnd

group :: Parser Expr
group = do
  i <- get
  modify (+1)
  res <- between (char '(') (char ')') expr
  return $ Group i res

token' :: Parser Expr
token' = start <|> end <|> wildcard <|> atom <|> try escaped <|> backRef
  where
    start = char '^' *> return Start
    end = char '$' *> return End
    wildcard = char '.' *> return Wildcard
    atom = Atom <$> noneOf specialChars
    escaped = Atom <$> (char '\\' *> oneOf specialChars)
    backRef = BackRef <$> (char '\\' *> int)

range :: Parser Expr
range = OneOf . fmap Atom <$> possibleChars
  where ranges = concat <$> some (try span' <|> lit)
        possibleChars = between (char '[') (char ']') ranges
        lit = (:[]) <$> noneOf "]"
        span' = do
          start <- noneOf "]"
          _ <- char '-'
          end <- noneOf "]"
          return [start..end]
