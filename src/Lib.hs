module Lib
  ( parseRegex
  , specialChars
  ) where

import Text.Read (readMaybe)
import Text.Parsec
import Text.Parsec.String

data Regex
  = Wildcard
  | Atom Char
  | Range [CharClass]
  | Start
  | End
  | Group [Regex]
  | Star
  | Plus
  | Optional
  | Repetition Int
               (Maybe Int)
  | BackRef Int
  deriving (Show)

data CharClass
  = Singleton Char
  | Span Char
         Char
  deriving (Show)

specialChars :: String
specialChars = "\\.[]{}()*+?|^$"

parseRegex :: String -> Either ParseError [Regex]
parseRegex = runParser regex () "Regex"

regex :: Parser [Regex]
regex = many $ choice [escaped, group, anchor, wildcard, range, star, plus, opt, repetition, atom]

star :: Parser Regex
star = char '*' *> return Star

plus :: Parser Regex
plus = char '+' *> return Plus

opt :: Parser Regex
opt = char '?' *> return Optional

repetition :: Parser Regex
repetition =
  between (char '{') (char '}') $
  do start <- read <$> many1 digit
     end <-
       option Nothing $
       do _ <- char ','
          spaces
          readMaybe <$> many digit
     return $ Repetition start end

wildcard :: Parser Regex
wildcard = char '.' >> return Wildcard

anchor :: Parser Regex
anchor = start <|> end
  where
    start = char '^' *> return Start
    end = char '$' *> return End

group :: Parser Regex
group = Group <$> between (char '(') (char ')') regex

range :: Parser Regex
range = Range <$> between (char '[') (char ']') (many1 (try span' <|> lit))
  where
    lit = Singleton <$> noneOf "]"
    span' = do
      start <- noneOf "]"
      _ <- char '-'
      end <- noneOf "]"
      return $ Span start end

atom :: Parser Regex
atom = Atom <$> anyChar

escaped :: Parser Regex
escaped = char '\\' *> (backRef <|> atom)
  where
    backRef = BackRef . read <$> many1 digit
