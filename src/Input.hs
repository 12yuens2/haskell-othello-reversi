module Input where

{-# LANGUAGE MultiWayIf #-}

import Prelude hiding (writeFile, readFile)
import System.Environment
import Data.ByteString.Lazy
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss

import Network.Socket hiding (sendAll, recv)
import Network.Socket.ByteString.Lazy
import Data.Binary

import Board
import AI
import Draw
import System.IO.Unsafe (unsafePerformIO)

-- import Network.Socket hiding (sendAll, recv)
-- import Network.Socket.ByteString.Lazy
-- import Data.Binary

import Debug.Trace

-- Update the world state given an input event. Some sample input events
-- are given; when they happen, there is a trace printed on the console
--
-- trace :: String -> a -> a
-- 'trace' returns its second argument while printing its first argument
-- to stderr, which can be a very useful way of debugging!
--handleInput :: Event -> World -> World
----handleInput (EventMotion (x, y)) w 
----    = trace ("Mouse moved to: " ++ show (x,y)) w
--handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) (World (Board sz ps pc) t sts bt wt v True go)
--    | x' < 0 || x' >= sz || y' < 0 || y' >= sz = (World (Board sz ps pc) t sts bt wt v True go) 
--    | otherwise
--    = case (startMove (Board sz ps pc) (x', y') t) of
--           Just b  -> trace ("Left button pressed at: " ++ show (x', y')) (World b (other t) (((Board sz ps pc),t):sts) bt wt v (startState (pieces b)) go)
--           Nothing -> trace ("Invalid move. Left button pressed at: " ++ show (x', y')) (World (Board sz ps pc) t sts bt wt v True go)
--    where x' = snapX sz x
--          y' = snapY sz y

--handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) (World (Board sz ps pc) t sts bt wt v r go)
--    | x' < 0 || x' >= sz || y' < 0 || y' >= sz = (World (Board sz ps pc) t sts bt wt v r go) 
--    | otherwise
--    = case (makeMove (Board sz ps pc) (x', y') t) of
--        Just b  -> trace ("Left button pressed at: " ++ show (x', y')) (World b (other t) (((Board sz ps pc),t):sts) bt wt v r go)
--        Nothing -> trace ("Invalid move. Left button pressed at: " ++ show (x', y')) (World (Board sz ps pc) t sts bt wt v r go)
--    where x' = snapX sz x
--          y' = snapY sz y

--handleInput (EventKey (Char k) Down _ _) w
--    = trace ("Key " ++ show k ++ " down") w
--handleInput (EventKey (Char k) Up _ _) (World b t sts bt wt v r go) 
--		| k == 'h' 	= (World b t sts bt wt (not v) r go)
--		| k == 'u'  = undoTurn (World b t sts bt wt v r go)
--		| k == 'r' 	= let args = unsafePerformIO $ getArgs in
--						 (initWorld args)
--	  	| otherwise = (World b t sts bt wt v r go)
--handleInput e w = w





--IO version of handle input
handleInputIO :: Event -> World -> IO World
handleInputIO (EventKey (MouseButton LeftButton) Up m (x, y)) (World (Board sz ps pc) t sts bt wt btime wtime p v True go sd sk)
    | x' < 0 || x' >= sz || y' < 0 || y' >= sz = return (World (Board sz ps pc) t sts bt wt btime wtime p v True go sd sk) 
    | otherwise
    = case (startMove (Board sz ps pc) (x', y') t) of
           Just b -> case sk of
                      Just s  -> trace ("Left button pressed at: " ++ show (x', y')) $
                                     do sendAll s (encode b)
                                        return (World b (other t) ((Board sz ps pc,t,btime,wtime):sts) bt wt btime wtime p v (startState (pieces b)) go sd sk)
                      Nothing -> trace ("Left button pressed at: " ++ show (x', y')) $ return (World b (other t) ((Board sz ps pc,t,btime,wtime):sts) bt wt btime wtime p v (startState (pieces b)) go sd sk)
           Nothing -> trace ("Invalid move. Left button pressed at: " ++ show (x', y')) $ return (World (Board sz ps pc) t sts bt wt btime wtime p v True go sd sk)
    where x' = snapX sz x
          y' = snapY sz y

--client acts as normal
handleInputIO (EventKey (MouseButton LeftButton) Up m (x, y)) (World (Board sz ps pc) t sts bt wt btime wtime p v r go sd sk)
    | x' < 0 || x' >= sz || y' < 0 || y' >= sz || p = return (World (Board sz ps pc) t sts bt wt btime wtime p v r go sd sk)
    | otherwise
    = case (makeMove (Board sz ps pc) (x', y') t) of
        Just b -> case sk of
                   Just s  -> trace ("Left button pressed at: " ++ show (x', y')) $ 
                                  do sendAll s (encode b)
                                     return (World b (other t) (((Board sz ps pc),t,btime,wtime):sts) bt wt btime wtime p v r go sd sk)
                   Nothing -> trace ("Left button pressed at: " ++ show (x', y')) $ 
                                  return (World b (other t) (((Board sz ps pc),t,btime,wtime):sts) bt wt btime wtime p v r go sd sk)
        Nothing -> trace ("Invalid move. Left button pressed at: " ++ show (x', y')) $ return (World (Board sz ps pc) t sts bt wt btime wtime p v r go sd sk)
    where x' = snapX sz x
          y' = snapY sz y

{-
--server sends move on click
handleInputIO s True (EventKey (MouseButton LeftButton) Up m (x, y)) (World (Board sz ps pc) t sts bt wt btime wtime p v r go)
    | x' < 0 || x' >= sz || y' < 0 || y' >= sz || p = return (World (Board sz ps pc) t sts bt wt btime wtime p v r go) 
    | otherwise
    = case (makeMove (Board sz ps pc) (x', y') t) of
        Just b  -> trace ("Left button pressed at: " ++ show (x', y')) $ 
          do
            let outputByteString = encode (World b (other t) (((Board sz ps pc),t,btime,wtime):sts) (othert bt) (othert wt) btime wtime p v r go)
              in sendAll s outputByteString
            return (World b (other t) (((Board sz ps pc),t,btime,wtime):sts) bt wt btime wtime p v r go)
        Nothing -> trace ("Invalid move. Left button pressed at: " ++ show (x', y')) $ return (World (Board sz ps pc) t sts bt wt btime wtime p v r go)
    where x' = snapX sz x
          y' = snapY sz y
-}
handleInputIO (EventKey (Char k) Down _ _) w
    = trace ("Key " ++ show k ++ " down") return w
handleInputIO (EventKey (Char k) Up _ _) w@(World b t sts bt wt btime wtime p v r go sd sk) 
        | k == 'h' && (not p) && (not go) = return w { showValid = not (showValid w) }
        | k == 'u' && (not p) && (not go) && sk == Nothing = return $ undoTurn w
        | k == 'p'            && (not go) && sk == Nothing = return w { pause = not (pause w) }
        | k == 'r' && (not p) && sk == Nothing = let args = unsafePerformIO $ getArgs in
                                                 initWorld args
        | k == 's' && (not p) && (not go) && sk == Nothing = do writeFile "save.othello" (encode w)
                                                                return w
        | k == 'l' && (not p) && (not go) && sk == Nothing = do fromFile <- readFile "save.othello"
                                                                return $ decode fromFile
        | otherwise           = return w
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

