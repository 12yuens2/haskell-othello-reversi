module Draw(drawWorldBMP, width, gridPos, rectSize, pieceSize) where

import Graphics.Gloss
import Board
import Data.Tuple

import Debug.Trace
import System.IO.Unsafe (unsafePerformIO)

width = 800.0
height = 800.0
gridPos = width/2.0

-- | Calculates the width of a single square based on board size
rectSize :: Int    -- ^ The size of the board/number of squares in a row
         -> Float  -- ^ returns the width of a single box
rectSize size = width / (fromIntegral size)

-- | Calculates the radius of a piece based on the board size
pieceSize :: Int    -- ^ The size of the board/number of squares in a row
          -> Float  -- ^ returns the radius of a piece
pieceSize size = ((rectSize size)/ 2.0)

-- | Translate between our custom 'Col' and the 'Color' needed for drawing
getCol :: Col -> Color
getCol Black = black
getCol White = white

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

-- | Loads bmp picture of black piece
blackPiece :: Picture
blackPiece = bmp "res/black2.bmp"

-- | Loads bmp picture of white piece
whitePiece :: Picture 
whitePiece = bmp "res/white2.bmp"

-- | Loads bmp picture of blank space
blankPiece :: Picture
blankPiece = bmp "res/blank.bmp"

-- | Loads bmp picture of hint piece
hintPiece :: Picture
hintPiece = bmp "res/hint2.bmp"

-- | Draws everything in the world
drawWorldBMP :: World    -- ^ The world to draw
             -> Picture
drawWorldBMP (World (Board size passes pieces) turn _ _ _ True True) = 
       pictures [ (drawBoardBMP size)
                , (drawHints size (checkStart (Board size passes pieces)))
                , (drawPiecesBMP size pieces)
				, drawText ("Black: " ++ (show $ evaluate (Board size passes pieces) Black)) (-2300) (-500)
				, drawText ("White: " ++ (show $ evaluate (Board size passes pieces) White)) (-2300) 0
                ]
drawWorldBMP (World (Board size passes pieces) turn _ _ _ True _) = 
       pictures [ (drawBoardBMP size)
                , (drawHints size (checkAvailable (Board size passes pieces) (0,0) turn))
                , (drawPiecesBMP size pieces)
				, drawText ("Black: " ++ (show $ evaluate (Board size passes pieces) Black)) (-2300) (-500)
				, drawText ("White: " ++ (show $ evaluate (Board size passes pieces) White)) (-2300) 0
                ]
drawWorldBMP (World (Board size passes pieces) _ _ _ _ _ _) = 
	pictures 	[ (drawBoardBMP size)
				, (drawPiecesBMP size pieces)
				, drawText ("Black: " ++ (show $ evaluate (Board size passes pieces) Black)) (-2300) (-500)
				, drawText ("White: " ++ (show $ evaluate (Board size passes pieces) White)) (-2300) 0
				]


-- | Draws the board
drawBoardBMP :: Int      -- ^ The size of the board
             -> Picture
drawBoardBMP size = pictures (map (drawEmptyPiece size) [(x,y) | x <- [0..(size-1)], y <- [0..(size-1)]])



-- | Draws text
drawText :: String   -- ^ String to draw
         -> Float    -- ^ x coordinate to draw at
         -> Float    -- ^ y coordinate to draw at
         -> Picture
drawText text x y = scale 0.25 0.25 $ translate x y $ Text text


-- | Draw hint pieces on the board
drawHints :: Int         -- ^ The size of the board on which hints are being drawn
          -> [Position]  -- ^ The list of positions to put hint pieces
          -> Picture
drawHints _ [] = Blank
drawHints size ((x,y):ps) = pictures [ (translate (squarePosX size $ fromIntegral(x)) (squarePosY size $ fromIntegral(y)) $ scale (scaleFactor size) (scaleFactor size) hintPiece)
                                , (drawHints size ps)
                                ]

-- | Draw an empty space onto the board
drawEmptyPiece :: Int       -- ^ The size of the board
               -> Position  -- ^ The position draw the empty space on the board
               -> Picture
drawEmptyPiece size (x,y) = translate (squarePosX size $ fromIntegral(x)) (squarePosY size $ fromIntegral(y)) $ scale (scaleFactor size) (scaleFactor size) blankPiece


-- | Draws pieces onto the boards
drawPiecesBMP :: Int                -- ^ The size of the board 
              -> [(Position, Col)]  -- ^ List of positions and colours for drawing pieces
              -> Picture
drawPiecesBMP _ [] = Blank
drawPiecesBMP size (((x,y),col):ps) = pictures [drawPieceBMP size (squarePosX size $ fromIntegral(x)) (squarePosY size $ fromIntegral(y)) col, drawPiecesBMP size ps]

-- | Draw a single piece onto the board
drawPieceBMP :: Int      -- ^ Size of the board
             -> Float    -- ^ x position to draw piece
             -> Float    -- ^ y position to draw piece
             -> Col      -- ^ colour of piece that is to be drawn
             -> Picture
drawPieceBMP size x y Black = translate x y $ scale (scaleFactor size) (scaleFactor size) blackPiece
drawPieceBMP size x y White = translate x y $ scale (scaleFactor size) (scaleFactor size) whitePiece
