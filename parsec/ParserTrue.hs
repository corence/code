module Main where

import Text.ParserCombinators.Parsec
import Control.Applicative hiding ((<|>), optional, many)

alwaysTrue :: Parser Bool
alwaysTrue = return True

main = print $ parse alwaysTrue "description string, ignore me" "I will be parsed"
