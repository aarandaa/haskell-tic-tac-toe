module Main where

-- gloss library for game graphics
-- https://hackage.haskell.org/package/gloss-1.13.2.2/docs/Graphics-Gloss.html
import Graphics.Gloss
import Graphics.Gloss.Data.Color

import GameLogic
import Render
import State

window = InWindow "Tic-Tac-Toe" (800,800) (100, 100)

bgColor = makeColor 255 255 255 255

main :: IO ()
main = play window bgColor 60 initialState stateToPicture nextState (const id)
