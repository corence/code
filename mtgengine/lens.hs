
{-# LANGUAGE TemplateHaskell #-}

import Control.Lens hiding (element)

data Water = Water { _left :: Atom, _right :: Atom, _base :: Atom } deriving (Show)

data Atom = Atom { _element :: String, _point :: Point } deriving (Show)

data Point = Point { _x :: Double, _y :: Double } deriving (Show)

makeLenses ''Atom
makeLenses ''Point
makeLenses ''Water

shiftAtomX :: Atom -> Atom
shiftAtomX = over (point . x) (+ 1)

pos :: Water -> Double
pos water = water ^. left ^. point ^. x

main = pure ()
