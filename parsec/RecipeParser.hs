{-# START_FILE Types.hs #-}
module Types where

data Recipe = Recipe
  { recipeName :: String
  , ingredients :: [Ingredient]
  , steps :: [Step]
  } deriving Show

type Measure = String

data Ingredient = Ingredient 
  { ingredientName :: String
  , quantity :: Int
  , measure :: Maybe Measure
  } deriving Show

data Step = Step
  { stepName :: String
  , order :: Int
  , stepDuration :: Maybe Duration
  } deriving (Eq, Show)

instance Ord Step where
    compare s1 s2 = compare (order s1) (order s2)

data Duration = Duration
  { duration :: Int
  , durationMeasure :: Measure
  } deriving (Eq, Show)


{-# START_FILE DSLParser.hs #-}
{-# LANGUAGE OverloadedStrings #-}

module DSLParser where

import Text.ParserCombinators.Parsec
import Types
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


(<||>) :: Parser a -> Parser a -> Parser a
p <||> q = try p <|> q


-- Here we are saying, try to match one between
-- gr and ml, if you can't default to Nothing.
-- The trick is using  pure :: a -> f a
measureP :: Parser (Maybe String)
measureP = (string "gr" *> (pure . Just $ "gr"))
       <|> (string "ml" *> (pure . Just $ "ml"))
       <|> (string "spoon" *> (pure . Just $ "spoon"))
       <|> (string "cup" *> (pure . Just $ "cup"))
       <|> (pure Nothing)


syntacticSugar :: String -> Parser (Maybe String)
syntacticSugar s = (string s *> (pure . Just $ s)) <|> pure Nothing


ingredient :: Parser Ingredient
ingredient = do
    qt <- lexeme int
    ms <- lexeme measureP
    lexeme (syntacticSugar "of")
    name <- lexeme stringLike
    lexeme (syntacticSugar "and")
    string "\r\n"
    return $ Ingredient name qt ms

-- Step
-------------------------------------------------------------------------------
step :: Parser Step
step = do
    sn <- lexeme stringLike
    d <- optionMaybe durationP
    lexeme (syntacticSugar "and")
    string "\r\n" <||> pure ""
    return $ Step sn 1 d

-- Duration
-------------------------------------------------------------------------------
durationP :: Parser Duration
durationP = do
    lexeme (string "for")
    d <- lexeme int
    u <- lexeme durationUnit
    return $ Duration d u
  where durationUnit = string "seconds" <|> string "minutes" <|> string "hours"


-- Recipe
-------------------------------------------------------------------------------
recipe :: Parser Recipe
recipe = do
    rn <- lexeme stringLike
    lexeme (syntacticSugar "is made with") *> string "\r\n"
    i <- many1 ingredient
    many1 (string "\r\n")
    lexeme (string "prepared by") *> string "\r\n"
    s <- many1 step
    return $ Recipe rn i s

{-# START_FILE Main.hs #-}
module Main where

import DSLParser
import Text.ParserCombinators.Parsec

initialDsl = unlines [
              "\"Ciambellone\" is made with\r",
              "    250 gr of \"Flour\"\r",
              "    250 gr of \"Sugar\"\r",
              "    130 ml of \"Sunflower Oil\"\r",
              "    130 ml of \"Water\"\r",
              "    3 \"Eggs\"\r",
              "\r",
              "  prepared by\r",
              "    \"Mixing everything\" and\r",
              "    \"Cooking in oven at 200 degrees\" for 40 minutes"]

main :: IO ()
main = print $ parse recipe "" initialDsl
