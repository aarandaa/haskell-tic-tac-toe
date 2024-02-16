module Render where

import Graphics.Gloss
import State
import Data.Array

pXColor = makeColorI 255 50 50 255 -- RED
pOColor = makeColorI 50 50 255 255 --  BLUE
drawColor = makeColorI 180 180 180 255 -- GREY
boardColor = makeColorI 0 0 0 255 -- BLACK


outcomeColor (Just PO) = pOColor
outcomeColor (Just PX) = pXColor
outcomeColor Nothing = drawColor

xTile :: Picture
xTile = pictures [ rotate 45.0 $ rectangleSolid side 10.0
                 , rotate (-45.0) $ rectangleSolid side 10.0
                 ] 
    where side = min tileW tileH * 0.75

oTile :: Picture
oTile = thickCircle radius 10.0
    where radius = min tileW tileH * 0.25

-- Changes tile
pictureToTile picture (row, col) = translate x y picture
    where x = fromIntegral col * tileW + tileW * 0.5
          y = fromIntegral row * tileH + tileH * 0.5

-- Edits an individual tile picture based on game move
tiles :: Board -> Tile -> Picture -> Picture
tiles board tile tilePic = 
    pictures
    $ map (pictureToTile tilePic . fst)
    $ filter(\(_, e) -> e == tile)
    $ assocs board



xTiles :: Board -> Picture 
xTiles board = tiles board (Full PX) xTile

oTiles :: Board -> Picture
oTiles board = tiles board (Full PO) oTile


grid :: Picture
grid = 
    pictures
    $ concatMap (\i -> [ line [ (i * tileW, 0.0)
                              , (i * tileW, fromIntegral screenH)
                              ]
                       , line [ (0.0, i * tileH)
                              , (fromIntegral screenW, i * tileH)
                              ]
                        ])
       [0.0 .. fromIntegral n]


boardToPicture board = 
    pictures [ xTiles board
             , oTiles board
             , grid
             ]

boardGameDone winner board = color (outcomeColor winner) (boardToPicture board)

boardGameOn board =
    pictures [ color pXColor $ xTiles board
             , color pOColor $ oTiles board
             , color boardColor $ grid
             ]

stateToPicture :: State -> Picture
stateToPicture state = translate (fromIntegral screenW * (-0.5))
                                 (fromIntegral screenH * (-0.5))
                                 frame
    where frame = case status state of
                    GameOn -> boardGameOn (board state)
                    GameDone winner -> boardGameDone winner (board state)