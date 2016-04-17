module Input where

import Prelude hiding (writeFile, readFile)
import System.Environment
import Data.ByteString.Lazy hiding (length)
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss

import Network.Socket hiding (sendAll, recv)
import Network.Socket.ByteString.Lazy
import Data.Binary

import Board
import AI
import Draw

import Debug.Trace


-- | Selects movement function to use based on whether placing start pieces or not
getMover :: Bool  -- ^ Boolean representing whether starting pieces are being placed
         -> (Board -> Position -> Col -> Maybe Board)  -- ^ movement function
getMover True = startMove
getMover False = makeMove


-- | Update the world given an input, the event is traced in console window
handleInputIO :: Event     -- ^ Event to react to
              -> World     -- ^ World to update
              -> IO World  -- ^ Returns updated world

-- For placing starting pieces
handleInputIO (EventKey (MouseButton LeftButton) Up m (x, y)) 
              w@(World b t sts _ _ btime wtime p _ r go _ sk)
    | x' < 0 || x' >= (size b) || y' < 0 || y' >= (size b) || p || go = return w
    | otherwise
    = do let moveFunc = getMover r
         case (moveFunc b (x', y') t) of
           Just b' -> case sk of
                      Just s  -> trace ("Left button pressed at: " ++ show (x', y')) $
                                 do sendAll s (encode b')
                                    return w {board = b', 
                                              turn = (other t), 
                                              stateList = (b,t,btime,wtime):sts,
                                              chooseStart = (length (pieces b') < 4)
                                             }
                      Nothing -> trace ("Left button pressed at: " ++ show (x', y')) $
                                 return w {board = b', 
                                           turn = (other t), 
                                           stateList = (b,t,btime,wtime):sts, 
                                           chooseStart = (length (pieces b') < 4)
                                          }
           Nothing -> trace ("Invalid move. Left button pressed at: " ++ show (x', y')) $ return w
    where x' = snapX (size b) x
          y' = snapY (size b) y


-- Handle key inputs
handleInputIO (EventKey (Char k) Down _ _) w
    = trace ("Key " ++ show k ++ " down") return w
handleInputIO (EventKey (Char k) Up _ _) w@(World _ _ _ _ _ _ _ p v _ go _ sk) 
        | k == 'h' && (not p) && (not go)                  = return w { showValid = not v }
        | k == 'u' && (not p) && (not go) && sk == Nothing = return $ undoTurn w
        | k == 'p'            && (not go) && sk == Nothing = return w { pause = not p }
        | k == 'r' && (not p) && sk == Nothing             = do args <- getArgs
                                                                initWorld args
        | k == 's' && (not p) && (not go) && sk == Nothing = do writeFile "save.othello" (encode w)
                                                                return w
        | k == 'l' && (not p) && (not go) && sk == Nothing = do fromFile <- readFile "save.othello"
                                                                return $ decode fromFile
        | otherwise           = return w
handleInputIO e w = return w


--Snaps the x mouse coordinate to the x grid coordinate
--snapX = floor((x + gridPos)/rectSize)
snapX :: Int    -- ^ The width of the board in squares
      -> Float  -- ^ the positon to convert to grid coordinate
      -> Int    -- ^ Returns the grid coordinate
snapX s x = floor((x + gridPos)/(rectSize s))

--Snaps the mouse coordinate to the y grid coordinate
--snapY = floor((gridPos - y)/rectSize)
snapY :: Int    -- ^ The width of the board in squares
      -> Float  -- ^ The position to convert to grid coordinates
      -> Int    -- ^ Returns the grid coordinate
snapY s y = floor((gridPos - y)/(rectSize s))

