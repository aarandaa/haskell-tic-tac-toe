module GameLogic where

import State
import Graphics.Gloss.Interface.Pure.Game
import Data.Array

isCoordValid = inRange ((0,0), (n-1, n-1))

switchPlayer state =
    case player state of
        PX -> state {player = PO}
        PO -> state {player = PX}

gameWin :: Player -> Board -> Bool 
gameWin player board = any isWin projs
    where vecs = allRows
                  ++ allCols
                  ++ allDiags
          allRows  = [[(i,j) | i <- [0..n-1]] | j <- [0..n-1]]
          allCols  = [[(j,i) | i <- [0..n-1]] | j <- [0..n-1]]
          allDiags = [[(i,i) | i <- [0..n-1]]
                  ,[(i,j) | i <- [0..n-1], let j = n-1-i ]]
          
          isWin vecs = (n ==)
                       $ length
                       $ filter (\cell -> cell == Full player)
                       $ map(\coord -> board ! coord) proj

countTiles :: Tile -> Board -> Int
countTiles tile = length . filter ((==) tile) . elems

isGameDone state
    | gameWin PX gameBoard =
        state { status = GameDone $ Just PX }
    | gameWin PO gameBoard =
        state { status = GameDone $ Just PO }
    | countTiles Empty gameBoard == 0 =
        state { status = GameDone Nothing }
    | otherwise = state
    where gameBoard = board state

turn :: State -> (Int, Int) -> State
turn state coord 
    | isCoordValid coord && gameBoard ! coord == Empty =
        isGameDone
        $ switchPlayer 
        $ state { board = gameBoard // [(coord, Full $ currentPlayer)]}
    | otherwise = state
    where gameBoard = board state
          currentPlayer = player state

mouseToCoord :: (Float, Float) -> (Int, Int)
mouseToCoord (x, y) = ( floor((y + (fromIntegral screenH * 0.5)) / tileH)
                     , floor((x + (fromIntegral screenW * 0.5)) / tileW)
                     )

nextState (EventKey (MouseButton LeftButton) Up _ mousePos) state = 
    case status state of
        GameOn -> turn state $ mouseToCoord mousePos
        GameDone _ -> initialState

nextState _ state = state