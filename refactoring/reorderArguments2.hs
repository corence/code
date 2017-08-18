import Text.ParserCombinators.Parsec
import Data.List

whitespace :: GenParser Char st String
whitespace = many (oneOf " \t")

a <&> b = b <$> a
a & b = b $ a

data Expression = Token String
                | Array Expression
                | Paren Expression
                | Infix String Expression Expression

instance Show Expression where
    show (Token s) = s
    show (Array e) = "[" ++ show e ++ "]"
    show (Paren e) = "(" ++ show e ++ ")"
    show (Infix s lhs rhs) = "{" ++ s ++ ", " ++ show lhs ++ ", " ++ show rhs ++ "}"

eol = try (string "\r\n")
  <|> try (string "\n\r")
  <|> string "\n"
  <|> string "\r"
  <?> "end of line"

expressions :: GenParser Char st [Expression]
expressions = sepBy expression eol

expression :: GenParser Char st Expression
expression = try infixExpression <|> singleExpression <?> "expression"

singleExpression :: GenParser Char st Expression
singleExpression = tokenExpression <|> arrayExpression <|> parenExpression <?> "singleExpression"

operatorToken :: GenParser Char st String
operatorToken = many1 (oneOf "->$:")

tokenExpression :: GenParser Char st Expression
tokenExpression = many1 alphaNum <&> Token

arrayExpression :: GenParser Char st Expression
arrayExpression = do
    char '['
    content <- expression
    char ']'
    pure $ Array content

parenExpression :: GenParser Char st Expression
parenExpression = do
    char '('
    content <- expression
    char ')'
    pure $ Paren content

infixExpression :: GenParser Char st Expression
infixExpression = do
    lhs <- singleExpression
    whitespace
    operator <- operatorToken
    whitespace
    rhs <- expression
    pure $ Infix operator lhs rhs

main = case parse expressions "wtf happened" input of
    Left badNews -> putStrLn $ "Bad news: " ++ show badNews
    Right results -> do
        putStrLn $ "Before twiddle:"
        putStrLn $ map render results & unlines
        putStrLn $ map show results & unlines
        putStrLn $ "After twiddle:"
        putStrLn $ map twiddle results & map render & unlines

-- if the expression is a function type signature with at least 2 args, swap the first 2
twiddle :: Expression -> Expression
twiddle statement = case statement of
    Infix ":" name (Infix "->" a (Infix "->" b c)) -> Infix ":" name (Infix "->" b (Infix "->" a c))
    otherwise -> statement

render :: Expression -> String
render (Token s) = s
render (Array e) = "[" ++ render e ++ "]"
render (Paren e) = "(" ++ render e ++ ")"
render (Infix s lhs rhs) = render lhs ++ " " ++ s ++ " " ++ render rhs

input = "deleteFromList : [a] -> Int -> [a]\nrepeat : Int -> a -> [a]\noneArg : Int -> Bool\nthreeArg : String -> Float -> Bool -> Int"
