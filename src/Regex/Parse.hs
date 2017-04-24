module Regex.Parse
  ( parseRegex
  , Structure(..)
  ) where

import Text.Read (readMaybe)
import Text.Parsec
import Text.Parsec.String

data Structure
  = Group Structure
  | Terms [Structure]
  | OneOf [Structure]
  | Repetition Structure Int (Maybe Int)
  | BackRef Int
  | Range String
  | Wildcard
  | Atom Char
  | Start
  | End
  deriving (Show)

specialChars :: String
specialChars = "\\[]|{}().+*?"

parseRegex :: String -> Either ParseError Structure
parseRegex = runParser regex () "Regex"

regex :: Parser Structure
regex = structure <* eof

structure :: Parser Structure
structure = OneOf <$> terms `sepBy1` char '|'

terms :: Parser Structure
terms = Terms <$> many1 term

term :: Parser Structure
term = do
  re <- choice [group, range, token']
  option re (quantifier re <|> repetition re)

quantifier :: Structure -> Parser Structure
quantifier re = do
  c <- oneOf "?*+"
  return $ case c of
             '?' -> Repetition re 0 (Just 1)
             '*' -> Repetition re 0 Nothing
             '+' -> Repetition re 1 Nothing
             _ -> error "Impossible pattern match fail"

repetition :: Structure -> Parser Structure
repetition re =
  between (char '{') (char '}') $ do
    start <- read <$> many1 digit
    end <- option Nothing $ do
      _ <- char ','
      spaces
      readMaybe <$> many digit
    return $ Repetition re start end

group :: Parser Structure
group = Group <$> between (char '(') (char ')') structure

token' :: Parser Structure
token' = start <|> end <|> wildcard <|> atom <|> try escaped <|> backRef
  where
    start = char '^' *> return Start
    end = char '$' *> return End
    wildcard = char '.' *> return Wildcard
    atom = Atom <$> noneOf specialChars
    escaped = Atom <$> (char '\\' *> oneOf specialChars)
    backRef = BackRef . read <$> (char '\\' *> many1 digit)

range :: Parser Structure
range = Range . concat <$> between (char '[') (char ']') (many1 (try span' <|> lit))
  where
    lit = (:[]) <$> noneOf "]"
    span' = do
      start <- noneOf "]"
      _ <- char '-'
      end <- noneOf "]"
      return [start..end]
