
module SignpostPuzzles
( puzzle3
, puzzle5
, puzzle7
) where

import Signpost
import Puzzle

type Direction = String

{-
puzzle1 = Map.fromList [
    ("a1", ("a1" 0 ["a3"] ["b1"])),
    ("a2", ("a2" 1 ["b1"] [])),
    ("a3", ("a3" 0 ["b2"] ["a1"])),
    ("b1", ("b1" 0 ["a1"] ["a2"])),
    ("b2", ("b2" 0 ["b3"] ["a3"])),
    ("b3", ("b3" 6 [] ["b2"]))
    ]
    -}

puzzle3 = Puzzle [
    ("a1", 1, ["a2", "a3"]),
    ("a2", 0, ["b3"]),
    ("a3", 0, ["b2", "c1"]),
    ("b1", 0, ["c1"]),
    ("b2", 0, ["c2"]),
    ("b3", 0, ["b2", "b1"]),
    ("c1", 0, ["c2", "c3"]),
    ("c2", 0, ["b2", "a2"]),
    ("c3", 9, [])
    ]

puzzle5 = Puzzle [
    ("a1", 0, ["a2", "a3", "a4", "a5"]),
    ("a2", 0, ["b3", "c4", "d5"]),
    ("a3", 0, ["a4", "a5"]),
    ("a4", 0, ["b3", "c2", "d1"]),
    ("a5", 16, ["a4", "a3", "a2", "a1"]),
    
    ("b1", 0, ["b2", "b3", "b4", "b5"]),
    ("b2", 0, ["c1"]),
    ("b3", 0, ["c4", "d5"]),
    ("b4", 0, ["b5"]),
    ("b5", 0, ["c4", "d3", "e2"]),
    
    ("c1", 25, []),
    ("c2", 0, ["d3", "e4"]),
    ("c3", 1, ["d2", "e1"]),
    ("c4", 10, ["c5"]),
    ("c5", 0, ["d4", "e3"]),
    
    ("d1", 0, ["e1"]),
    ("d2", 0, ["c2", "b2", "a2"]),
    ("d3", 0, ["d4", "d5"]),
    ("d4", 0, ["b2", "a1"]),
    ("d5", 0, ["e4"]),

    ("e1", 0, ["e2", "e3", "e4", "e5"]),
    ("e2", 0, ["d2", "c2", "b2", "a2"]),
    ("e3", 12, ["e4", "e5"]),
    ("e4", 0, ["d3", "c2", "b1"]),
    ("e5", 0, ["d4", "b2", "a1"])
    ]

puzzle7 = [
    ("a1", 1, "se"),
    ("a2", 0, "e"),
    ("a3", 0, "s"),
    ("a4", 0, "e"),
    ("a5", 43, "sw"),
    ("a6", 0, "w"),
    ("a7", 47, "s"),

    ("b1", 0, "s"),
    ("b2", 0, "w"),
    ("b3", 0, "se"),
    ("b4", 0, "sw"),
    ("b5", 0, "e"),
    ("b6", 0, "s"),
    ("b7", 0, "s"),
    
    ("c1", 0, "s"),
    ("c2", 41, "ne"),
    ("c3", 0, "se"),
    ("c4", 0, "s"),
    ("c5", 0, "w"),
    ("c6", 0, "s"),
    ("c7", 48, "s"),

    ("d1", 0, "ne"),
    ("d2", 0, "se"),
    ("d3", 0, "nw"),
    ("d4", 0, "ne"),
    ("d5", 19, "nw"),
    ("d6", 0, "w"),
    ("d7", 0, "s"),

    ("e1", 0, "se"),
    ("e2", 22, "se"),
    ("e3", 0, "ne"),
    ("e4", 0, "w"),
    ("e5", 3, "nw"),
    ("e6", 0, "n"),
    ("e7", 0, "w"),

    ("f1", 0, "ne"),
    ("f2", 0, "ne"),
    ("f3", 16, "ne"),
    ("f4", 0, "n"),
    ("f5", 0, "se"),
    ("f6", 11, "n"),
    ("f7", 0, "w"),

    ("g1", 28, "e"),
    ("g2", 0, "nw"),
    ("g3", 0, "e"),
    ("g4", 0, "n"),
    ("g5", 0, "n"),
    ("g6", 0, "nw"),
    ("g7", 49, "")
    ] :: [ProtoCell]
