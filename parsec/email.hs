import Text.ParserCombinators.Parsec

data URI = URI {
    uri_protocol :: String,
    uri_host :: String,
    uri_port :: Int,
    uri_path :: [String],
    uri_query :: [(String, String)]
}

uri :: GenParser Char st URI
uri = do
    p_protocol <- protocol
    p_host <- host
    p_port <- port p_protocol
    p_path <- path
    p_query <- query
    return URI {
        uri_protocol = p_protocol,
        uri_host = p_host,
        uri_port = p_port,
        uri_path = p_path,
        uri_query = p_query
        }

protocol :: GenParser Char st String
protocol =
    try (do
        result <- many (noneOf ":") -- [^:]*
        string "://"
        return result)
    <|> string ""
    <?> "protocol"

host :: GenParser Char st String
host = many (noneOf ":/") -- [^:/]*

port :: String -> GenParser Char st Int
port protocol = (char ':' >> read (many1 digit))
                <|> case protocol of
                        "http" -> return 80
                        "https" -> return 443
                        _ -> fail ("unrecognized protocol " ++ protocol)
                
                --case protocol of
                               --"http" -> 80
                               --"https" -> 443
                               --_ -> 0

path :: GenParser Char st [String]
path = char '/' >> sepBy (many (noneOf "/")) (char '/') -- something like /([^/]*/)* where / is nonmagic

query :: GenParser Char st [(String, String)]
query = (char '?' >> sepBy (queryPair) (char '&'))
        <|> return []

queryPair :: GenParser Char st (String, String)
queryPair = do
    key <- many (noneOf "&=") 
    char '='
    value <- many (noneOf "&=")
    return (key, value)

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

