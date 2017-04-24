module Regex.Parse
  ( parseRegex
  , RegexToken(..)
  ) where

import Text.Read (readMaybe)
import Text.Parsec
import Text.Parsec.String

data RegexToken
  = Wildcard
  | Atom Char
  | Range [CharClass]
  | Start
  | End
  | Group [RegexToken]
  | Star
  | Plus
  | Optional
  | Alternation
  | Repetition Int
               (Maybe Int)
  | BackRef Int
  deriving (Show)

data CharClass
  = Singleton Char
  | Span Char
         Char
  deriving (Show)

parseRegex :: String -> Either ParseError [RegexToken]
parseRegex = runParser regex () "RegexToken"

regex :: Parser [RegexToken]
regex = many $ choice [escaped, group, anchor, wildcard, range, alternation, star, plus, opt, repetition, atom]

alternation :: Parser RegexToken
alternation = char '|' *> return Alternation

star :: Parser RegexToken
star = char '*' *> return Star

plus :: Parser RegexToken
plus = char '+' *> return Plus

opt :: Parser RegexToken
opt = char '?' *> return Optional

repetition :: Parser RegexToken
repetition =
  between (char '{') (char '}') $
  do start <- read <$> many1 digit
     end <-
       option Nothing $
       do _ <- char ','
          spaces
          readMaybe <$> many digit
     return $ Repetition start end

wildcard :: Parser RegexToken
wildcard = char '.' >> return Wildcard

anchor :: Parser RegexToken
anchor = start <|> end
  where
    start = char '^' *> return Start
    end = char '$' *> return End

group :: Parser RegexToken
group = Group <$> between (char '(') (char ')') regex

range :: Parser RegexToken
range = Range <$> between (char '[') (char ']') (many1 (try span' <|> lit))
  where
    lit = Singleton <$> noneOf "]"
    span' = do
      start <- noneOf "]"
      _ <- char '-'
      end <- noneOf "]"
      return $ Span start end

atom :: Parser RegexToken
atom = Atom <$> anyChar

escaped :: Parser RegexToken
escaped = char '\\' *> (backRef <|> atom)
  where
    backRef = BackRef . read <$> many1 digit
