module AI where

import System.Exit
import Network.Socket hiding (sendAll, recv)
import Network.Socket.ByteString.Lazy
import Data.Binary

import Board
import Datatype

import Debug.Trace

import Data.Maybe

data GameTree = GameTree { game_board :: Board,
                           game_turn :: Col,
                           next_moves :: [(Position, GameTree)] }


-- | generates all possible moves that can be made for a colour on a board
generateMoves :: Board       -- ^ The board to check
              -> Col         -- ^ The colour to check valid moves for
              -> [Position]  -- ^ Returns a list of valid moves
generateMoves b c = checkAvailable b (0,0) c

-- | Given a function to generate plausible moves (i.e. board positions)
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
             -> Board                     -- ^ board state
             -> Col                       -- ^ player to play next
             -> GameTree                  -- ^ returns a tree of possible moves and the boards they create
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
yusukiMove :: Int       -- ^ Maximum search depth
           -> GameTree  -- ^ Initial game tree
           -> Position  -- ^ Returns position of the best move
yusukiMove (-1) tree = fst (head (next_moves tree))
yusukiMove 0 (GameTree b c []) = undefined
yusukiMove depth (GameTree b c next_moves) = 
    bestPos depth c next_moves 
    where
      bestPos :: Int -> Col -> [(Position, GameTree)] -> Position
      bestPos depth c next_moves = snd $ maximum $ childrenList (depth-1) next_moves c


-- | Creates a list of scores for a list of next moves in a Gametree 
childrenList :: Int                     -- ^ Offset from maximum depth (0 is maximum specified depth) 
             -> [(Position, GameTree)]  -- ^ List of moves and the GameTrees they create
             -> Col                     -- ^ colour of Ai player to evaluate for
             -> [(Int, Position)]       -- ^ Returns a list of positions (moves) and scores associated with them
childrenList _ [] _ = []
childrenList depth ((p,tree):xs) c = (evaluateChildren depth tree c c, p):childrenList depth xs c


-- | Gets a score for a Gameteree and max depth or otherwise gets min/max of its children
evaluateChildren  :: Int       -- ^ Offset from maximum depth (0 is maximum specified depth) 
                  -> GameTree  -- ^ Gametree to evaluate children of
                  -> Col       -- ^ Colour of AI player to evaluate for
                  -> Col       -- ^ Colour of current turn
                  -> Int       -- ^ Returns a score for this child or min/max of its children's scores
evaluateChildren 0     GameTree {game_board = b}  target _       = yusukiEvaluate b target
evaluateChildren depth GameTree {next_moves = ms} target current | target == current = getMin $ makeList depth ms target current
                                                                 | otherwise         = getMax $ makeList depth ms target current

-- | creates a list of scores for each GameTree in a list of next moves
makeList :: Int                      -- ^ Current depth in tree
         -> [(Position, GameTree)]   -- ^ list of positions and gametrees they create
         -> Col                      -- ^ Colour of AI player to evaluate for
         -> Col                      -- ^ Colour of current turn
         -> [Int]                    -- ^ Returns a list of max/min scores of children
makeList _ [] _ _= []
makeList depth ((_,tree):xs) target current 
                 = evaluateChildren (depth-1) tree target (other current):makeList depth xs target current

-- | Gets minimum from score list to choose opponent's move, 
-- assumes that opponent having no moves is very good
getMax :: [Int]   -- ^ List of scores of possible moves
       -> Int     -- ^ returns maximum in list or preset value for empty list
getMax [] = -10000 
getMax list = maximum list

-- | Gets minimum from score list to choose opponent's move, 
-- assumes that opponent having no moves is very good
getMin :: [Int]  -- ^ List of scores of possiblr moves
       -> Int    -- ^ returns minimum in list or preset value for empty list
getMin [] = 10000
getMin list = minimum list


-- | Updates the world based on its current state
updateWorldIO :: Float     -- ^ Time passed so far
              -> World     -- ^ The world to check state of
              -> IO World  -- ^ Returns updated world
updateWorldIO _ w@(World b c sts bt wt btime wtime p v r go sd sk) 
    | (not r && gameOver b) || btime <= 0 || wtime <= 0 = return w {gameIsOver = True}
    | not (r || validMovesAvailable b c) = trace ("No valid moves for " ++ show c ++ " so their turn is skipped") 
                                           $ return w {board = b{passes = passes b + 1}, turn = other c}
    | p || (r && isNothing sk) || (isJust sk && (sd && c == Black || not sd && c == White)) = return w
    | isJust sk = 
        do let s = fromJust sk
           inputByteString <- recv s 65536
           let b' = decode inputByteString
           return $ w {board = b', 
                       turn = other c, 
                       chooseStart = length (pieces b') < 4
                      }
    | c == Black && bt == Human = return w {bTimer = btime - 10}
    | c == White && wt == Human = return w {wTimer = wtime - 10}
    | otherwise = let
        tree = buildTree generateMoves b c
        nextMove = yusukiMove 5 tree in case makeMove b nextMove c of
                                  Nothing -> error "not possible moves not implemented"
                                  Just b' -> return $ w {board = b', turn = other c}
