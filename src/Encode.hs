import Data.Binary
import Board

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
  put Human  = put (0 :: Word8)
  put AI     = put (1 :: Word8)
  put Server = put (2 :: Word8)
  put Client = put (3 :: Word8)
  get = do t <- get :: Get Word8
           case t of 
            0 -> return Human
            1 -> return AI 
            2 -> return Server
            3 -> return Client 

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