
module JsonParser where

import Text.Parsec
import qualified Data.Map as Map
import Data.Map(Map)

data Value
  = Number Float
  | Words String
  | Object (Map String Value)
  | List [Value]

parseObject :: Parser Value
parseObject = do
    char '{'
    many whitespace
    records <- many parseObjectRecord
    char '}'
    many whitespace
    pure $ Object (Map.fromList records)

parseObjectRecord :: Parser (String, Value)
parseObjectRecord = do
    key <- parseString
    char ':'
    many whitespace
    value <- parseValue
    many whitespace
    pure $ (show key, value)

parseString :: Parser Value
-- parseString = Words <$> (char '"' *> many quotedChar <* char '"')
parseString = do
    char '"'
    string <- many quotedChar
    char '"'
    many whitespace
    pure $ Words string
    where quotedChar = try "\\\"" <|> noneOf "\"\\" <?> "char in string"

parseList :: Parser Value
parseList = do
    char '['
    values <- sepBy parseValue (char ',' >> many whitespace)
    char ']'
    many whitespace
    pure $ List values

parseValue :: Parser Value
parseValue
  =   parseString
  <|> parseNumber
  <|> parseList
  <|> parseObject
  <?> "any value"
