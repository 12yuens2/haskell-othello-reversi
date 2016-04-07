module Draw(drawWorldBMP) where

import Graphics.Gloss
import Board
import Data.Tuple

import Debug.Trace
import System.IO.Unsafe (unsafePerformIO)

width = 1200.0
height = 800.0
gridPos = height/2.0
rectSize = height / (fromIntegral sizeOfBoard)
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
boardDrawing (World (Board size passes pieces) turn _ _ True) = pictures [background, 
                                                                          validDrawing (checkAvailable (Board size passes pieces) (0,0) turn),
                                                                          gridDrawing,
                                                                          (piecesDrawing pieces)
                                                                         ]
boardDrawing (World (Board size passes pieces) turn _ _ _)    = pictures [background,
                                                                          gridDrawing,
                                                                          (piecesDrawing pieces)
                                                                         ]


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

-- | Highlights all valid moves
validDrawing :: [Position]  -- ^ The positions of valid moves to be highlighted
             -> Picture
validDrawing [] = Blank
validDrawing (x:xs) = pictures [drawValid (getPosX x) (getPosY x), (validDrawing xs)]

-- | Draws a square highlighting a valid ove
drawValid  :: Float   -- ^ x coordinate of valid square
           -> Float   -- ^ y coordinate of valid square
           -> Picture
drawValid x y = (color rose (polygon [(squarePosX x, squarePosY y), (squarePosX (x+1), squarePosY y), (squarePosX (x+1), squarePosY (y+1)), (squarePosX x, squarePosY (y+1))])) 

-- | converts an x position to an x position on the GUI for squares
squarePosX :: Float -- ^ The position to be converted
           -> Float -- ^ Returns the converted position
squarePosX x = -gridPos + (rectSize * x) 

-- | converts an y position to an y position on the GUI for squares
squarePosY :: Float -- ^ The position to be converted 
           -> Float -- ^ Returns the converted position
squarePosY y = gridPos - (rectSize * y) 


-- | Draw an individual vertical line given an offset
-- The line is drawn at rectSize*offset
drawVertial :: Float 	-- ^ The offset where the line is drawn.
			-> Picture
drawVertial x = color black (line [((squarePosX x), gridPos), ((squarePosX x), -(gridPos)) ])


-- | Draw an individual horizontal line given an offset
-- The line is drawn at rectSize*offset
drawHorizontal 	:: Float 	-- ^ The offset where the line is drawn.
				-> Picture
drawHorizontal y = color black (line [(gridPos, (squarePosY y)), (-(gridPos), (squarePosY y)) ])



-- | Translate the x board coordinate to the x coordinate on the gui
guiX :: Float -> Float
guiX x = -gridPos + pieceSize + (rectSize*x)

-- | Translate the y board coordinate to the y coordinate on the gui
guiY :: Float -> Float
guiY y = gridPos - pieceSize - (rectSize*y)





{- Drawing with Bitmap images -}



bmp :: FilePath -> Picture
bmp file = unsafePerformIO $ loadBMP file

blackPiece :: Picture
blackPiece = bmp "res/black.bmp"

whitePiece :: Picture 
whitePiece = bmp "res/white.bmp"

blankPiece :: Picture
blankPiece = bmp "res/blank.bmp"

hintPiece :: Picture
hintPiece = bmp "res/hint.bmp"

drawWorldBMP :: World -> Picture
drawWorldBMP (World (Board size passes pieces) turn _ _ True) = pictures [ (drawBoardBMP size)
                                                                      , (drawHints (checkAvailable (Board size passes pieces) (0,0) turn))
                                                                      , (drawPiecesBMP pieces)
                                                                      ]

drawWorldBMP (World (Board size passes pieces) _ _ _ _) = 
	pictures 	[ (drawBoardBMP size)
				, (drawPiecesBMP pieces)
				, drawText ("Black: " ++ (show $ evaluate (Board size passes pieces) Black)) (-2300) (-500)
				, drawText ("White: " ++ (show $ evaluate (Board size passes pieces) White)) (-2300) 0
				]


drawBoardBMP :: Int -> Picture
drawBoardBMP size = pictures (map drawEmptyPiece [(x,y) | x <- [0..(size-1)], y <- [0..(size-1)]])



drawText :: String -> Float -> Float -> Picture
drawText text x y = scale 0.25 0.25 $ translate x y $ Text text


drawHints :: [Position] -> Picture
drawHints [] = Blank
drawHints ((x,y):ps) = pictures [ (translate (guiX $ fromIntegral(x)) (guiY $ fromIntegral(y)) $ hintPiece)
                                , (drawHints ps)
                                ]

drawEmptyPiece :: Position -> Picture
drawEmptyPiece (x,y) = translate (guiX $ fromIntegral(x)) (guiY $ fromIntegral(y)) $ blankPiece


drawPiecesBMP :: [(Position, Col)] -> Picture 
drawPiecesBMP [] = Blank
drawPiecesBMP (((x,y),col):ps) = pictures [drawPieceBMP (guiX $ fromIntegral(x)) (guiY $ fromIntegral(y)) col, drawPiecesBMP ps]

drawPieceBMP :: Float -> Float -> Col -> Picture
drawPieceBMP x y Black = translate x y $ blackPiece
drawPieceBMP x y White = translate x y $ whitePiece