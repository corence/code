-- file: ch16/csv1.hs
import Text.ParserCombinators.Parsec

csvFile :: GenParser Char st [[String]]
csvFile = endBy line eol

line :: GenParser Char st [String]
line = sepBy cell (char ',')

cell, quotedCell, unquotedCell :: GenParser Char st String
cell = quotedCell <|> unquotedCell

quotedCell = char '"' >> many quotedChar >>= (\result -> (char '"' <?> "quote at end of cell") >> return result)

quotedChar = noneOf ("\"")
             <|> (try (string "\"\"") >> return '"')

unquotedCell = many (noneOf ",\n\r")

eol = try (string "\r\n")
  <|> try (string "\n\r")
  <|> string "\n"
  <|> string "\r"
  <?> "end of line"

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input

main = putStrLn $ show $ parseCSV "Name,Age,Location\nLacey,15,Summer\nWinter,14,Cherry\n"
