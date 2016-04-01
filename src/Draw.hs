module Draw(drawWorld) where

import Graphics.Gloss
import Board
import Data.Tuple

import Debug.Trace

width = 800.0
height = 800.0
gridPos = width/2.0
rectSize = width / (fromIntegral sizeOfBoard)
pieceSize = (rectSize / 2.0)

-- | Translate between our custom 'Col' and the 'Color' needed for drawing
getCol :: Col -> Color
getCol Black = black
getCol White = white

-- Given a world state, return a Picture which will render the world state.
-- Currently just draws a single blue circle as a placeholder.
--
-- This will need to extract the Board from the world state and draw it
-- as a grid plus pieces.
drawWorld :: World -> Picture
drawWorld w = boardDrawing w


-- | Draws entire board.
-- The picture consists of: the background, the grid, the pieces
boardDrawing :: World -> Picture
boardDrawing (World (Board size passes pieces) turn) = pictures [background, gridDrawing, (piecesDrawing pieces)]


-- | Draws a green rectangle as the background
background :: Picture
background = Color (makeColor8 0 102 51 255) $ rectangleSolid width height


-- | Draws grid by drawing horizontal and vertical lines
gridDrawing :: Picture
gridDrawing = pictures [horizontalDrawing, verticalDrawing]


-- | Draw all pieces given a list of (Position, Col)
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



-- | Draw an individual piece (circle)
drawPiece 	:: Float 	-- ^ x coordinate of the piece
			-> Float 	-- ^ y coordinate of the piece
			-> Color 	-- ^ Color of the piece
			-> Picture 
drawPiece x y c = translate (guiX x) (guiY y) $ Color c $ circleSolid pieceSize

-- | Draw an individual vertical line given an offset
-- The line is drawn at rectSize*offset
drawVertial :: Float 	-- ^ The offset where the line is drawn.
			-> Picture
drawVertial x = color black (line [(-(gridPos)+(rectSize*x), gridPos), (-(gridPos)+(rectSize*x), -(gridPos)) ])


-- | Draw an individual horizontal line given an offset
-- The line is drawn at rectSize*offset
drawHorizontal 	:: Float 	-- ^ The offset where the line is drawn.
				-> Picture
drawHorizontal y = color black (line [(gridPos, -(gridPos)+(rectSize*y)), (-(gridPos), -(gridPos)+(rectSize*y)) ])



-- | Translate the x board coordinate to the x coordinate on the gui
guiX :: Float -> Float
guiX x = -gridPos + pieceSize + (rectSize*x)

-- | Translate the y board coordinate to the y coordinate on the gui
guiY :: Float -> Float
guiY y = gridPos - pieceSize - (rectSize*y)