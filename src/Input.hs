module Input where

{-# LANGUAGE MultiWayIf #-}

import System.Environment

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Board
import AI
import Draw
import System.IO.Unsafe (unsafePerformIO)

import Network.Socket hiding (sendAll, recv)
import Network.Socket.ByteString.Lazy
import Data.Binary

import Debug.Trace

-- Update the world state given an input event. Some sample input events
-- are given; when they happen, there is a trace printed on the console
--
-- trace :: String -> a -> a
-- 'trace' returns its second argument while printing its first argument
-- to stderr, which can be a very useful way of debugging!
handleInputIO :: Event -> World -> IO World
--handleInput (EventMotion (x, y)) w 
--    = trace ("Mouse moved to: " ++ show (x,y)) w
handleInputIO (EventKey (MouseButton LeftButton) Up m (x, y)) (World (Board sz ps pc) t sts bt wt btime wtime p v True go)
    | x' < 0 || x' >= sz || y' < 0 || y' >= sz || p = return (World (Board sz ps pc) t sts bt wt btime wtime p v True go)
    | otherwise
    = case (startMove (Board sz ps pc) (x', y') t) of
           Just b  -> trace ("Left button pressed at: " ++ show (x', y')) $ return (World b (other t) ((Board sz ps pc,t,btime,wtime):sts) bt wt btime wtime p v (startState (pieces b)) go)
           Nothing -> trace ("Invalid move. Left button pressed at: " ++ show (x', y')) $ return (World (Board sz ps pc) t sts bt wt btime wtime p v True go)
    where x' = snapX sz x
          y' = snapY sz y

handleInputIO (EventKey (MouseButton LeftButton) Up m (x, y)) (World (Board sz ps pc) t sts bt wt btime wtime p v r go)
    | x' < 0 || x' >= sz || y' < 0 || y' >= sz || p = return (World (Board sz ps pc) t sts bt wt btime wtime p v r go) 
    | otherwise
    = case (makeMove (Board sz ps pc) (x', y') t) of
        Just b  -> trace ("Left button pressed at: " ++ show (x', y')) $ return (World b (other t) ((Board sz ps pc,t,btime,wtime):sts) bt wt btime wtime p v r go)
        Nothing -> trace ("Invalid move. Left button pressed at: " ++ show (x', y')) $ return (World (Board sz ps pc) t sts bt wt btime wtime p v r go)
    where x' = snapX sz x
          y' = snapY sz y

handleInputIO (EventKey (Char k) Down _ _) w
        = return $ trace ("Key " ++ show k ++ " down") w
handleInputIO (EventKey (Char k) Up _ _) (World b t sts bt wt btime wtime p v r go) 
        | k == 'h' && (not p) && (not go) = return (World b t sts bt wt btime wtime p (not v) r go)
        | k == 'u' && (not p) && (not go) = return $ undoTurn (World b t sts bt wt btime wtime p v r go)
        | k == 'p'            && (not go) = return (World b t sts bt wt btime wtime (not p) v r go)
        | k == 'r' && (not p) = let args = unsafePerformIO $ getArgs in
                                return (initWorld args)
        | otherwise           = return (World b t sts bt wt btime wtime p v r go)
handleInputIO e w = return w

--Snaps the x mouse coordinate to the x grid coordinate
--snapX = floor((x + gridPos)/rectSize)
snapX :: Int -> Float -> Int
snapX s x = floor((x + gridPos)/(rectSize s))

--Snaps the mouse coordinate to the y grid coordinate
--snapY = floor((gridPos - y)/rectSize)
snapY :: Int -> Float -> Int
snapY s y = floor((gridPos - y)/(rectSize s))

{- Hint: when the 'World' is in a state where it is the human player's
-}

