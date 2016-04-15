{-# LANGUAGE MultiWayIf #-}

module AI where

import System.Exit

import Board

import Debug.Trace

data GameTree = GameTree { game_board :: Board,
                           game_turn :: Col,
                           next_moves :: [(Position, GameTree)] }


genAllMoves :: Board -> Col -> [Position]
genAllMoves b c = [(x,y) | x <- [0..(size b - 1)], y <- [0..(size b - 1)]]


generateMoves :: Board -> Col -> [Position]
generateMoves b c = checkAvailable b (0,0) c

-- Given a function to generate plausible moves (i.e. board positions)
-- for a player (Col) on a particular board, generate a (potentially)
-- infinite game tree.
--
-- (It's not actually infinite since the board is finite, but it's sufficiently
-- big that you might as well consider it infinite!)
--
-- An important part of the AI is the 'gen' function you pass in here.
-- Rather than generating every possible move (which would result in an
-- unmanageably large game tree!) it could, for example, generate moves
-- according to various simpler strategies.

buildTree :: (Board -> Col -> [Position]) -- ^ Move generator
             -> Board -- ^ board state
             -> Col -- ^ player to play next
             -> GameTree
buildTree gen b c = let moves = gen b c in -- generated moves
                        GameTree b c (mkNextStates moves)
  where
    mkNextStates :: [Position] -> [(Position, GameTree)]
    mkNextStates [] = []
    mkNextStates (pos : xs)
        = case makeMove b pos c of -- try making the suggested move
               Nothing -> mkNextStates xs -- not successful, no new state
               Just b' -> (pos, buildTree gen b' (other c)) : mkNextStates xs
                             -- successful, make move and build tree from 
                             -- here for opposite player

-- Get the best next move from a (possibly infinite) game tree. This should
-- traverse the game tree up to a certain depth, and pick the move which
-- leads to the position with the best score for the player whose turn it
-- is at the top of the game tree.

-- | Have Yusuki (the greatest Othello player in the world) make a move.
yusukiMove :: Int -- ^ Maximum search depth
            -> GameTree -- ^ Initial game tree
            -> Position
yusukiMove (-1) tree = fst (head (next_moves tree))
yusukiMove 0 (GameTree b c []) = undefined
yusukiMove depth (GameTree b c next_moves) = trace ("makeing best move possible") $ 
    bestPos depth c next_moves 
    where
      --snd $ maximum [(value, pos) | value <- evaluate ((makeMove b ) c), pos <- next_poss]

      bestPos :: Int -> Col -> [(Position, GameTree)] -> Position
      bestPos depth c next_moves = trace ("--" ++ show(childrenList (depth-1) next_moves c)) snd $ maximum $ childrenList (depth-1) next_moves c


childrenList :: Int -> [(Position, GameTree)] -> Col -> [(Int, Position)]
childrenList _ [] _ = []
--childrenList depth ((p,tree):xs) c = ((evaluateChildren depth tree c (other c)), p):(childrenList depth xs c)
childrenList depth ((p,tree):xs) c = ((evaluateChildren depth tree c c), p):(childrenList depth xs c)


evaluateChildren  :: Int -- ^ Depth 
                  -> GameTree
                  -> Col       -- ^ target colour
                  -> Col       -- ^ current turn
                  -> Int
evaluateChildren 0     GameTree {game_board = b} target current = evaluate2 b target
evaluateChildren depth GameTree {next_moves = ms} target current | target == current = getMin $ makeList depth ms target current
                                                                 | otherwise         = getMax $ makeList depth ms target current

makeList :: Int                      -- ^ depth
         -> [(Position, GameTree)]   -- ^ list of positions and gametrees they create
         -> Col                      -- ^ Colour target
         -> Col                      -- ^ colour of current turn
         -> [Int]
makeList _ [] _ _= []
makeList depth ((_,tree):xs) target current = (evaluateChildren (depth-1) tree target (other current)):(makeList depth xs target current)

getMax :: [Int] -> Int
getMax [] = -100000   -- need to change
getMax list = maximum list

getMin :: [Int] -> Int
getMin [] = -100000  -- need to change
getMin list = minimum list

--yusukiMove 0 gametree = --get best score and return up the tree
--yusukiMove depth gametree = undefined --keep going down to the given depth

-- Update the world state after some time has passed
updateWorldIO :: Float -- ^ time since last update (you can ignore this)
              -> World -- ^ current world state
              -> IO World
updateWorldIO _ (World b c sts bt wt btime wtime p v True go) 
            = return $ World b c sts bt wt btime wtime p v True go
updateWorldIO _ (World b c sts bt wt btime wtime p v r go) 
            | gameOver b || btime <= 0 || wtime <= 0 = return $ World b c sts bt wt btime wtime p v r True
            | p                                      = return $ World b c sts bt wt btime wtime p v r go
            | not (validMovesAvailable b c) = return $ trace ("No valid moves for " ++ show c ++ " so their turn is skipped") World (b {passes = (passes b) + 1}) (other c) sts bt wt btime wtime p v False go
            | c == Black && bt == Human     = return $ World b {passes = 0} c sts bt wt (btime-10) wtime p v False go
            | c == White && wt == Human     = return $ World b {passes = 0} c sts bt wt btime (wtime-10) p v False go
            | otherwise = let
                          tree = buildTree generateMoves b c
                          nextMove = yusukiMove 5 tree in
                                     case makeMove b nextMove c of
                                          Nothing -> error("not possible moves not implemented")
                                          Just b' -> return $ (World (b' {passes = 0}) (other c) sts bt wt btime wtime p v False go)

{- Hint: 'updateWorld' is where the AI gets called. If the world state
 indicates that it is a computer player's turn, updateWorld should use
 'getBestMove' to find where the computer player should play, and update
 the board in the world state with that move.

 At first, it is reasonable for this to be a random valid move!

 If both players are human players, the simple version above will suffice,
 since it does nothing.

 In a complete implementation, 'updateWorld' should also check if either 
 player has won and display a message if so.
-}


