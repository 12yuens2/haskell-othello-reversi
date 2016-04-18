module Datatype where

import Data.Binary
import Network.Socket

type Position = (Int, Int)


-- | A Col is either Black or White and represents the two opposing players
data Col = Black | White
  deriving (Show, Eq)

-- | A Board is a record containing the board size (a board is a square grid, n *
-- n), the number of consecutive passes, and a list of pairs of position and
-- the colour at that position.
data Board = Board { size :: Int,
                     passes :: Int,
                     pieces :: [(Position, Col)]
                   }
  deriving Show

-- | PlayerType represents whether the player is Human or AI (more types
-- can be added in future for different AI types)
data PlayerType = Human 
                | AI
                | Random
                | Network
  deriving (Show, Eq)

-- Overall state is the board and whose turn it is, plus any further
-- information about the world (this may later include, for example, player
-- names, timers, information about rule variants, etc)
--
-- Feel free to extend this, and 'Board' above with anything you think
-- will be useful (information for the AI, for example, such as where the
-- most recent moves were).
data World = World { board :: Board,
                     turn :: Col,
                     stateList :: [(Board, Col, Int, Int)],
                     bType :: PlayerType,
                     wType :: PlayerType,
                     bTimer :: Int,
                     wTimer :: Int,
                     isPaused :: Bool,
                     showValid :: Bool,
                     chooseStart :: Bool,
                     gameIsOver :: Bool,
                     serverSide :: Bool,
                     sock       :: Maybe Socket
                     }
  deriving Show

-- Binary encoding and decoding for World
-- Note the socket is not encoded and is set to Nothing should it be decoded
instance Binary World where
  put (World b t sts bt wt btime wtime p v r go sd sk) = do 
                                        put b
                                        put t
                                        put sts
                                        put bt
                                        put wt
                                        put btime
                                        put wtime
                                        put p
                                        put v
                                        put r
                                        put go
                                        put sd
  get = do b <- get
           t <- get
           sts <- get
           bt <- get
           wt <- get
           btime <- get
           wtime <- get
           p <- get
           v <- get
           r <- get
           go <- get
           sd <- get
           return (World b t sts bt wt btime wtime p v r go sd Nothing)

-- Binary encoding and decoding for PlayerType
instance Binary PlayerType where
  put Human   = put (0 :: Word8)
  put AI      = put (1 :: Word8)
  put Random  = put (2 :: Word8)
  put Network = put (3 :: Word8)
  get = do t <- get :: Get Word8
           case t of 
            0 -> return Human
            1 -> return AI 
            2 -> return Random
            3 -> return Network

-- Binary encoding and decoding for colour
instance Binary Col where
  put Black = put (0 :: Word8)
  put White = put (1 :: Word8)
  get = do t <- get :: Get Word8
           case t of 
            0 -> return Black
            1 -> return White

-- Binary encoding and decoding for board
instance Binary Board where
  put (Board sz ps pc) = do put sz
                            put ps
                            put pc
  get = do sz <- get
           ps <- get
           pc <- get
           return (Board sz ps pc)
