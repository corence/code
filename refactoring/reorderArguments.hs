import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Data.List

whitespace :: GenParser Char st String
whitespace = many (oneOf " \t")

-- Expected result!
-- InfixExpression ":" (TokenExpression "deleteFromList") (InfixExpression "->" (ArrayExpression (TokenExpression "a")) (InfixExpression "->" (TokenExpression "Int") (ArrayExpression (TokenExpression "a"))))

a <&> b = b <$> a
a & b = b $ a

data Expression = TokenExpression String
                | ArrayExpression Expression
                | ParenExpression Expression
                | InfixExpression String Expression Expression

instance Show Expression where
    show (TokenExpression s) = s
    show (ArrayExpression e) = "[" ++ show e ++ "]"
    show (ParenExpression e) = "(" ++ show e ++ ")"
    show (InfixExpression s lhs rhs) = "{" ++ s ++ ", " ++ show lhs ++ ", " ++ show rhs ++ "}"

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
tokenExpression = many1 alphaNum <&> TokenExpression

arrayExpression :: GenParser Char st Expression
arrayExpression = do
    char '['
    content <- expression
    char ']'
    pure $ ArrayExpression content

parenExpression :: GenParser Char st Expression
parenExpression = do
    char '('
    content <- expression
    char ')'
    pure $ ParenExpression content

infixExpression :: GenParser Char st Expression
infixExpression = do
    lhs <- singleExpression 
    whitespace
    operator <- operatorToken
    whitespace
    rhs <- expression
    pure $ InfixExpression operator lhs rhs
    
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
    InfixExpression ":" name (InfixExpression "->" a (InfixExpression "->" b c)) -> InfixExpression ":" name (InfixExpression "->" b (InfixExpression "->" a c))
    otherwise -> statement

render :: Expression -> String
render (TokenExpression s) = s
render (ArrayExpression e) = "[" ++ render e ++ "]"
render (ParenExpression e) = "(" ++ render e ++ ")"
render (InfixExpression s lhs rhs) = render lhs ++ " " ++ s ++ " " ++ render rhs

input = "deleteFromList : [a] -> Int -> [a]\nrepeat : Int -> a -> [a]\noneArg : Int -> Bool\nthreeArg : String -> Float -> Bool -> Int"
