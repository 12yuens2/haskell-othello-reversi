module Draw(drawWorld, width, gridPos, rectSize, pieceSize) where

import Graphics.Gloss
import Board
import Data.Tuple

import Debug.Trace

width = 800.0
height = 800.0
gridPos = width/2.0

rectSize :: Int -> Float
rectSize size = width / (fromIntegral size)

pieceSize :: Int -> Float
pieceSize size = ((rectSize size)/ 2.0)

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
boardDrawing (World (Board size passes pieces) turn _ _ _ True) = pictures [background, 
                                                                            validDrawing size (checkAvailable (Board size passes pieces) (0,0) turn),
                                                                            gridDrawing size,
                                                                            (piecesDrawing size pieces)
                                                                           ]
boardDrawing (World (Board size passes pieces) turn _ _ _ _)    = pictures [background,
                                                                            gridDrawing size,
                                                                            (piecesDrawing size pieces)
                                                                           ]


-- | Draws a green rectangle as the background
background :: Picture
background = Color (makeColor8 0 102 51 255) $ rectangleSolid width height


-- | Draws grid by drawing horizontal and vertical lines
gridDrawing :: Int -> Picture
gridDrawing size = pictures [horizontalDrawing size, verticalDrawing size]


-- | Draw all pieces given a list of (Position, Col)
piecesDrawing :: Int -> [(Position, Col)] -> Picture
piecesDrawing s [] = Blank
piecesDrawing s (x:xs) = pictures 
	[ drawPiece s (getPosX (fst x)) (getPosY (fst x)) (getCol (snd x))
	, (piecesDrawing s xs)
	]


--map to draw lines for [1..7]
verticalDrawing :: Int -> Picture
verticalDrawing s = let size = (fromIntegral (s-1) :: Float) in
                        pictures (map (drawVertial s) [1.0..size])

horizontalDrawing :: Int -> Picture
horizontalDrawing s = let size = (fromIntegral (s-1) :: Float) in
                          pictures (map (drawHorizontal s) [1.0..size])



-- | Draw an individual piece (circle)
drawPiece 	:: Int
            -> Float 	-- ^ x coordinate of the piece
			-> Float 	-- ^ y coordinate of the piece
			-> Color 	-- ^ Color of the piece
			-> Picture 
drawPiece s x y c = translate (guiX s x) (guiY s y) $ Color c $ circleSolid (pieceSize s)

-- | Highlights all valid moves
validDrawing :: Int
             -> [Position]  -- ^ The positions of valid moves to be highlighted
             -> Picture
validDrawing _ [] = Blank
validDrawing s (x:xs) = pictures [drawValid s (getPosX x) (getPosY x), (validDrawing s xs)]

-- | Draw an individual vertical line given an offset
-- The line is drawn at rectSize*offset
drawVertial :: Int
            -> Float 	-- ^ The offset where the line is drawn.
			-> Picture
drawVertial s x = color black (line [((squarePosX s x), gridPos), ((squarePosX s x), -(gridPos)) ])


-- | Draw an individual horizontal line given an offset
-- The line is drawn at rectSize*offset
drawHorizontal 	:: Int
                -> Float 	-- ^ The offset where the line is drawn.
				-> Picture
drawHorizontal s y = color black (line [(gridPos, (squarePosY s y)), (-(gridPos), (squarePosY s y)) ])



-- | Draws a square highlighting a valid ove
drawValid  :: Int
           -> Float   -- ^ x coordinate of valid square
           -> Float   -- ^ y coordinate of valid square
           -> Picture
drawValid s x y = (color rose (polygon [(squarePosX s x, squarePosY s y), (squarePosX s (x+1), squarePosY s y), (squarePosX s (x+1), squarePosY s (y+1)), (squarePosX s x, squarePosY s (y+1))])) 

-- | converts an x position to an x position on the GUI for squares
squarePosX :: Int
           -> Float -- ^ The position to be converted
           -> Float -- ^ Returns the converted position
squarePosX s x = -gridPos + ((rectSize s)* x) 

-- | converts an y position to an y position on the GUI for squares
squarePosY :: Int
           -> Float -- ^ The position to be converted 
           -> Float -- ^ Returns the converted position
squarePosY s y = gridPos - ((rectSize s)* y) 





-- | Translate the x board coordinate to the x coordinate on the gui
guiX :: Int -> Float -> Float
guiX s x = -gridPos + (pieceSize s)+ ((rectSize s)*x)

-- | Translate the y board coordinate to the y coordinate on the gui
guiY :: Int -> Float -> Float
guiY s y = gridPos - (pieceSize s) - ((rectSize s)*y)
