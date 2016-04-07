module Draw(drawWorldBMP, width, gridPos, rectSize, pieceSize) where

import Graphics.Gloss
import Board
import Data.Tuple

import Debug.Trace
import System.IO.Unsafe (unsafePerformIO)

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

{-
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
drawPiece s x y c = translate (squarePosX s x) (squarePosY s y) $ Color c $ circleSolid (pieceSize s)

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





-- | Translate the x board coordinate to the x coordinate on the squarePos
squarePosX :: Int -> Float -> Float
squarePosX s x = -gridPos + (pieceSize s)+ ((rectSize s)*x)

-- | Translate the y board coordinate to the y coordinate on the squarePos
squarePosY :: Int -> Float -> Float
squarePosY s y = gridPos - (pieceSize s) - ((rectSize s)*y)
-}



{- Drawing with Bitmap images -}


-- | converts an x position to an x position on the GUI for squares
squarePosX :: Int
           -> Float -- ^ The position to be converted
           -> Float -- ^ Returns the converted position
squarePosX s x = -gridPos + (pieceSize s) + ((rectSize s)* x) 

-- | converts an y position to an y position on the GUI for squares
squarePosY :: Int
           -> Float -- ^ The position to be converted 
           -> Float -- ^ Returns the converted position
squarePosY s y = gridPos - (pieceSize s) - ((rectSize s)* y) 

defaultSquareWidth = 8

scaleFactor :: Int -> Float
scaleFactor size = defaultSquareWidth / fromIntegral(size)

bmp :: FilePath -> Picture
bmp file =  unsafePerformIO $ loadBMP file

blackPiece :: Picture
blackPiece = bmp "res/black.bmp"

whitePiece :: Picture 
whitePiece = bmp "res/white.bmp"

blankPiece :: Picture
blankPiece = bmp "res/blank.bmp"

hintPiece :: Picture
hintPiece = bmp "res/hint.bmp"

drawWorldBMP :: World -> Picture
drawWorldBMP (World (Board size passes pieces) turn _ _ _ True True) = pictures [ (drawBoardBMP size)
                                                                      , (drawHints size (checkStart (Board size passes pieces)))
                                                                      , (drawPiecesBMP size pieces)
                                                                      ]
drawWorldBMP (World (Board size passes pieces) turn _ _ _ True _) = pictures [ (drawBoardBMP size)
                                                                      , (drawHints size (checkAvailable (Board size passes pieces) (0,0) turn))
                                                                      , (drawPiecesBMP size pieces)
                                                                      ]

drawWorldBMP (World (Board size passes pieces) _ _ _ _ _ _) = 
	pictures 	[ (drawBoardBMP size)
				, (drawPiecesBMP size pieces)
				, drawText ("Black: " ++ (show $ evaluate (Board size passes pieces) Black)) (-2300) (-500)
				, drawText ("White: " ++ (show $ evaluate (Board size passes pieces) White)) (-2300) 0
				]


drawBoardBMP :: Int -> Picture
drawBoardBMP size = pictures (map (drawEmptyPiece size) [(x,y) | x <- [0..(size-1)], y <- [0..(size-1)]])



drawText :: String -> Float -> Float -> Picture
drawText text x y = scale 0.25 0.25 $ translate x y $ Text text


drawHints :: Int -> [Position] -> Picture
drawHints _ [] = Blank
drawHints size ((x,y):ps) = pictures [ (translate (squarePosX size $ fromIntegral(x)) (squarePosY size $ fromIntegral(y)) $ scale (scaleFactor size) (scaleFactor size) hintPiece)
                                , (drawHints size ps)
                                ]

drawEmptyPiece :: Int -> Position -> Picture
drawEmptyPiece size (x,y) = translate (squarePosX size $ fromIntegral(x)) (squarePosY size $ fromIntegral(y)) $ scale (scaleFactor size) (scaleFactor size) blankPiece


drawPiecesBMP :: Int -> [(Position, Col)] -> Picture 
drawPiecesBMP _ [] = Blank
drawPiecesBMP size (((x,y),col):ps) = pictures [drawPieceBMP size (squarePosX size $ fromIntegral(x)) (squarePosY size $ fromIntegral(y)) col, drawPiecesBMP size ps]

drawPieceBMP :: Int -> Float -> Float -> Col -> Picture
drawPieceBMP size x y Black = translate x y $ scale (scaleFactor size) (scaleFactor size) blackPiece
drawPieceBMP size x y White = translate x y $ scale (scaleFactor size) (scaleFactor size) whitePiece
