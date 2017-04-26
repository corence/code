module Main where

import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), optional, many)

ws :: Parser String
ws = many (oneOf " ")


int :: (Integral a, Read a) => Parser a
int = read <$> many1 digit


stringLike :: Parser String
stringLike = char '"' *> many (noneOf ['\"', '\r', '\n']) <* char '"'


-- A parser combinator which skips whitespaces from both sides
lexeme :: Parser a -> Parser a
lexeme p = ws *> p <* ws

main :: IO ()
main = do
    print $ parse int "" "10"
    putStrLn $ "---"
    print $ parse int "" "    10   "
    putStrLn $ "---"
    print $ parse (lexeme int) "" "   10 "
