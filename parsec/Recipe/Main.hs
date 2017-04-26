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


