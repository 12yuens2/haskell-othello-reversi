module Input where

import Prelude hiding (writeFile, readFile)
import System.Environment
import Data.ByteString.Lazy hiding (length)
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss

import Network.Socket hiding (sendAll, recv)
import Network.Socket.ByteString.Lazy
import Data.Binary
import Data.Maybe

import Board
import AI
import Draw
import Datatype

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

    -- if the position clicked is outside the board, don't do anything
    | x' < 0 || x' >= size b || y' < 0 || y' >= size b || p || go = return w
    | otherwise = trace ("Left button pressed at: " ++ show (x', y')) $ 
    
      -- Get a movement function and apply it
      do let moveFunc = getMover r
         case moveFunc b (x', y') t of

           -- return w' with the move made
           Just b' -> trace "Valid move" $
                      let w' = w { board = b', 
                                   turn = other t, 
                                   stateList = (b,t,btime,wtime):sts, 
                                   chooseStart = length (pieces b') < 4 } in
                        case sk of
                          -- Send the board across the socket if it exists
                          Just s  -> do sendAll s (encode b')
                                        return w'

                          -- Otherwise play the game normally
                          Nothing -> return w'

           -- makeMove function returned Nothing so it was an invalid move
           Nothing -> trace "Invalid move" $ return w
    where x' = snapX (size b) x
          y' = snapY (size b) y


-- Handle key inputs
handleInputIO (EventKey (Char k) Down _ _) w = trace ("Key " ++ show k ++ " down") return w
handleInputIO (EventKey (Char k) Up _ _) w@(World _ _ _ _ _ _ _ p v _ go _ sk) 
        -- Show hints if the game is not paused and not over
        | k == 'h' && not p && not go                   = return w { showValid = not v }

        -- Undo moves if the game is not paused and not networked
        | k == 'u' && not p && isNothing sk   = return $ undoTurn w

        -- Pause the game
        | k == 'p'          && not go && isNothing sk   = return w { isPaused = not p }

        -- Restart the game
        | k == 'r' && not p           && isNothing sk   = trace "Restarting game" $
                                                          do args <- getArgs
                                                             initWorld args

        -- Save the game to "save.othello"
        | k == 's' && not p && not go && isNothing sk   = trace "Saving game state" $
                                                          do writeFile "save.othello" (encode w)
                                                             return w

        -- Load the game from "save.othello"
        | k == 'l' && not p && isNothing sk   = trace "Loading saved game state" $
                                                          do fromFile <- readFile "save.othello"
                                                             return $ decode fromFile

        -- Unrecognised keys
        | otherwise                                     = return w
handleInputIO e w = return w


--Snaps the x mouse coordinate to the x grid coordinate
--snapX = floor((x + gridPos)/rectSize)
snapX :: Int    -- ^ The width of the board in squares
      -> Float  -- ^ the positon to convert to grid coordinate
      -> Int    -- ^ Returns the grid coordinate
snapX s x = floor((x + gridPos)/rectSize s)

--Snaps the mouse coordinate to the y grid coordinate
--snapY = floor((gridPos - y)/rectSize)
snapY :: Int    -- ^ The width of the board in squares
      -> Float  -- ^ The position to convert to grid coordinates
      -> Int    -- ^ Returns the grid coordinate
snapY s y = floor((gridPos - y)/rectSize s)

