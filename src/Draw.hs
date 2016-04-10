module Draw(drawWorldBMP, width, gridPos, rectSize, pieceSize) where

import Graphics.Gloss
import Board
import Data.Tuple

import Debug.Trace
import System.IO.Unsafe (unsafePerformIO)

width = 800.0
height = 800.0
gridPos = width/2.0

-- | Calculates the width of a single square based on board sz
rectSize :: Int    -- ^ The sz of the board/number of squares in a row
         -> Float  -- ^ returns the width of a single box
rectSize sz = width / (fromIntegral sz)

-- | Calculates the radius of a piece based on the board sz
pieceSize :: Int    -- ^ The sz of the board/number of squares in a row
          -> Float  -- ^ returns the radius of a piece
pieceSize sz = ((rectSize sz)/ 2.0)

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
scaleFactor sz = defaultSquareWidth / fromIntegral(sz)

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

gameOverScreen :: Picture
gameOverScreen = bmp "res/gameover.bmp"

-- ^ Draws the world and information based on its state
drawWorldBMP :: World    -- ^ The world which is to be drawn 
             -> Picture

--Draw gameover
drawWorldBMP (World (Board sz ps pieces) turn _ _ _ _ _ True ) = pictures [ gameOverScreen, (drawText (getWinner (Board sz ps pieces)) 0 0)]

-- Draws hints for reversi start
drawWorldBMP (World (Board sz ps pieces) turn _ _ _ True True _ ) = 
       pictures [ (drawBoardBMP sz)
                , (drawHints sz (checkStart (Board sz ps pieces)))
                , (drawPiecesBMP sz pieces)
				, drawText ("Black: " ++ (show $ evaluate (Board sz ps pieces) Black)) (-2300) (-500)
				, drawText ("White: " ++ (show $ evaluate (Board sz ps pieces) White)) (-2300) 0
                ]

--Draw hints
drawWorldBMP (World (Board sz ps pieces) turn _ _ _ True _ _) = 
       pictures [ (drawBoardBMP sz)
                , (drawHints sz (checkAvailable (Board sz ps pieces) (0,0) turn))
                , (drawPiecesBMP sz pieces)
				, drawText ("Black: " ++ (show $ evaluate (Board sz ps pieces) Black)) (-2300) (-500)
				, drawText ("White: " ++ (show $ evaluate (Board sz ps pieces) White)) (-2300) 0
                ]


-- Draw otherwise
drawWorldBMP (World (Board sz ps pieces) _ _ _ _ _ _ _ ) = 
	pictures 	[ (drawBoardBMP sz)
				, (drawPiecesBMP sz pieces)
				, drawText ("Black: " ++ (show $ evaluate (Board sz ps pieces) Black)) (-2300) (-500)
				, drawText ("White: " ++ (show $ evaluate (Board sz ps pieces) White)) (-2300) 0
				]


-- | Draws the empty board
drawBoardBMP :: Int      -- ^ The size of the board
             -> Picture
drawBoardBMP sz = pictures (map (drawEmptyPiece sz) [(x,y) | x <- [0..(sz-1)], y <- [0..(sz-1)]])


-- ^ Gets a string stating the outcome of a game based on a board
getWinner :: Board   -- The board to evaluate to find winner
          -> String  -- Returns a string stating the winner
getWinner b | x == y    = "Game is a draw"
            | x > y     = "Black wins!"
            | otherwise = "White wins!"
  where (x,y) = checkScore b


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
drawHints sz ((x,y):ps) = pictures [ (translate (squarePosX sz $ fromIntegral(x)) (squarePosY sz $ fromIntegral(y)) $ scale (scaleFactor sz) (scaleFactor sz) hintPiece)
                                , (drawHints sz ps)
                                ]

-- | Draw an empty space onto the board
drawEmptyPiece :: Int       -- ^ The size of the board
               -> Position  -- ^ The position draw the empty space on the board
               -> Picture
drawEmptyPiece sz (x,y) = translate (squarePosX sz $ fromIntegral(x)) (squarePosY sz $ fromIntegral(y)) $ scale (scaleFactor sz) (scaleFactor sz) blankPiece


-- | Draws pieces onto the boards
drawPiecesBMP :: Int                -- ^ The size of the board 
              -> [(Position, Col)]  -- ^ List of positions and colours for drawing pieces
              -> Picture
drawPiecesBMP _ [] = Blank
drawPiecesBMP sz (((x,y),col):ps) = pictures [drawPieceBMP sz (squarePosX sz $ fromIntegral(x)) (squarePosY sz $ fromIntegral(y)) col, drawPiecesBMP sz ps]

-- | Draw a single piece onto the board
drawPieceBMP :: Int      -- ^ The size of the board
             -> Float    -- ^ x position to draw piece
             -> Float    -- ^ y position to draw piece
             -> Col      -- ^ colour of piece that is to be drawn
             -> Picture
drawPieceBMP sz x y Black = translate x y $ scale (scaleFactor sz) (scaleFactor sz) blackPiece
drawPieceBMP sz x y White = translate x y $ scale (scaleFactor sz) (scaleFactor sz) whitePiece
