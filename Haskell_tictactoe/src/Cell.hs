module Cell where

import Player (Player)

-- You may wish to define your own Show instance
data Cell = Empty | Taken Player deriving Eq

instance Show Cell where
    show Empty     = "-"
    show (Taken p) = show p 

-- | Returns True if the given cell is Empty
isEmpty :: Cell -> Bool
isEmpty Empty = True
isEmpty _     = False
