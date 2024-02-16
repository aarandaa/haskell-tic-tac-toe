module State where

import Data.Array

n :: Int
n = 3

screenW :: Int
screenW = 700
screenH :: Int
screenH = 700

tileW :: Float
tileW = fromIntegral screenW / fromIntegral n

tileH :: Float
tileH = fromIntegral screenH / fromIntegral n

data Player = PX | PO deriving (Eq, Show)
type Board = Array (Int, Int) Tile
data Tile = Empty | Full Player deriving (Eq, Show)

-- If Player defined for GameDone status, thats the winner
-- If Player not defined for GameDone status, it is a tie
data Status = GameOn | GameDone (Maybe Player) deriving(Eq, Show)

data State = State { board :: Board 
                   , player :: Player
                   , status :: Status
                   } deriving(Eq, Show)      

initialState = State { board = (array indexRange $ zip (range indexRange) (cycle [Empty]))
                     , player = PO
                     , status = GameOn
                     }
    where indexRange = ((0,0), (2,2))
