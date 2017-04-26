module Regex.Parse
  ( parseRegex
  , Expr(..)
  ) where

import Text.Read (readMaybe)
import Text.Parsec
import Text.Parsec.String

data Expr
  = Group Expr
  | Terms [Expr]
  | OneOf [Expr]
  | Repetition Expr Int (Maybe Int)
  | BackRef Int
  | Range String
  | Wildcard
  | Atom Char
  | Start
  | End
  deriving (Show)

specialChars :: String
specialChars = "\\[]|{}().+*?"

parseRegex :: String -> Either ParseError Expr
parseRegex = runParser regex () "Regex"

regex :: Parser Expr
regex = expr <* eof

expr :: Parser Expr
expr = OneOf <$> terms `sepBy1` char '|'

terms :: Parser Expr
terms = Terms <$> many1 term

term :: Parser Expr
term = do
  re <- choice [group, range, token']
  option re (quantifier re <|> repetition re)

quantifier :: Expr -> Parser Expr
quantifier re = do
  c <- oneOf "?*+"
  return $ case c of
             '?' -> Repetition re 0 (Just 1)
             '*' -> Repetition re 0 Nothing
             '+' -> Repetition re 1 Nothing
             _ -> error "Impossible pattern match fail"

repetition :: Expr -> Parser Expr
repetition re =
  between (char '{') (char '}') $ do
    start <- read <$> many1 digit
    end <- option Nothing $ do
      _ <- char ','
      spaces
      readMaybe <$> many digit
    return $ Repetition re start end

group :: Parser Expr
group = Group <$> between (char '(') (char ')') expr

token' :: Parser Expr
token' = start <|> end <|> wildcard <|> atom <|> try escaped <|> backRef
  where
    start = char '^' *> return Start
    end = char '$' *> return End
    wildcard = char '.' *> return Wildcard
    atom = Atom <$> noneOf specialChars
    escaped = Atom <$> (char '\\' *> oneOf specialChars)
    backRef = BackRef . read <$> (char '\\' *> many1 digit)

range :: Parser Expr
range = Range . concat <$> between (char '[') (char ']') (many1 (try span' <|> lit))
  where
    lit = (:[]) <$> noneOf "]"
    span' = do
      start <- noneOf "]"
      _ <- char '-'
      end <- noneOf "]"
      return [start..end]
