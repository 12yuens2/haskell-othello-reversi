module Input(handleInput) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Board
import AI
import Draw

import Debug.Trace

-- Update the world state given an input event. Some sample input events
-- are given; when they happen, there is a trace printed on the console
--
-- trace :: String -> a -> a
-- 'trace' returns its second argument while printing its first argument
-- to stderr, which can be a very useful way of debugging!
handleInput :: Event -> World -> World
--handleInput (EventMotion (x, y)) w 
--    = trace ("Mouse moved to: " ++ show (x,y)) w
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) w 
    = trace ("Left button pressed at: " ++ show (x,y)) w
handleInput (EventKey (Char k) Down _ _) w
    = trace ("Key " ++ show k ++ " down") w
handleInput (EventKey (Char k) Up _ _) w
    = trace ("Key " ++ show k ++ " up") w
handleInput e w = w

{- Hint: when the 'World' is in a state where it is the human player's
 turn to move, a mouse press event should calculate which board position
 a click refers to, and update the board accordingly.

 At first, it is reasonable to assume that both players are human players.
-}

