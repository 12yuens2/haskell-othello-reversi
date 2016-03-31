module Draw(drawWorld) where

import Graphics.Gloss
import Board
import Data.Tuple

width = 800.0
height = 800.0
gridPos = width/2.0
rectSize = width / (fromIntegral sizeOfBoard)
pieceSize = (rectSize / 2.0)


getCol :: Col -> Color
getCol Black = black
getCol White = white

-- Given a world state, return a Picture which will render the world state.
-- Currently just draws a single blue circle as a placeholder.
--
-- This will need to extract the Board from the world state and draw it
-- as a grid plus pieces.
drawWorld :: World -> Picture
drawWorld w = boardDrawing


--draws entire board
boardDrawing :: Picture
boardDrawing = pictures [background, gridDrawing, (piecesDrawing (pieces initBoard))]


--draws a green rectangle as the background
background :: Picture
background = Color (makeColor8 0 102 51 255) $ rectangleSolid width height


--draws grid by drawing horizontal and vertical lines
gridDrawing :: Picture
gridDrawing = pictures [horizontalDrawing, verticalDrawing]


--draw pieces given a list of (position, color)
piecesDrawing :: [(Position, Col)] -> Picture
piecesDrawing [] = Blank
piecesDrawing (x:xs) = pictures 
	[ drawPiece (getPosX (fst x)) (getPosY (fst x)) (getCol (snd x))
	, (piecesDrawing xs)
	]

--map to draw lines for [1..7]
verticalDrawing :: Picture
verticalDrawing = pictures (map drawVertial [1.0..7.0])

horizontalDrawing :: Picture
horizontalDrawing = pictures (map drawHorizontal [1.0..7.0])



--drawing individual pieces
drawPiece :: Float -> Float -> Color -> Picture
drawPiece x y c = translate (guiX x) (guiY y) $ Color c $ circleSolid pieceSize

--translates the x and y from board to positions on the gui
guiX :: Float -> Float
guiX x = -gridPos + pieceSize + (rectSize*x)

guiY :: Float -> Float
guiY y = gridPos - pieceSize - (rectSize*y)

--drawing individual lines
drawVertial :: Float -> Picture
drawVertial x = color black (line [(-(width/2.0)+(rectSize*x), width/2.0), (-(width/2.0)+(rectSize*x), -(width/2.0)) ])

drawHorizontal :: Float -> Picture
drawHorizontal x = color black (line [(width/2.0, -(width/2.0)+(rectSize*x)), (-(width/2.0), -(width/2.0)+(rectSize*x)) ])