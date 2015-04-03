module Binary where

import Core
import Data.Binary
import qualified Data.Text as T
import Data.Text.Encoding
import Binary.UTCTime
import Control.Applicative

instance Binary T.Text where
  put = put . encodeUtf8
  get = decodeUtf8 <$> get

instance Binary UserMessage where
  put (UserMessage (msg, cntxt)) = do put msg
                                      put cntxt
  get = do msg <- get
           cntxt <- get
           return (UserMessage (msg,cntxt))

instance Binary IRCState where
  put (IRCState msgs usrs acts _ gen) = do
    put msgs
    put usrs
    put acts
    put $ show gen
  get = do msgs <- get
           usrs <- get
           acts <- get
           gen <- read <$> get
           return $ IRCState msgs usrs acts T.empty gen

instance Binary MessageContext where
  put (MessageContext nick full channel time) = do
    put nick
    put full
    put channel
    put time
  get = do
    nick <- get
    full <- get
    channel <- get
    time <- get
    return (MessageContext nick full channel time)

instance Binary IRCAction where
  put (PrivMsg msg)  = putWord8 0 >> put msg
  put (Debug  msg)   = putWord8 1 >> put msg
  put Pong           = putWord8 2
  put ReJoin         = putWord8 3
  put Leave          = putWord8 4
  put Quit           = putWord8 5
  put NoAction       = putWord8 6
  put (ChangeNick n) = putWord8 7 >> put n

  get = do
    t <- getWord8
    case t of
     0 -> PrivMsg <$> get
     1 -> Debug <$> get
     7 -> ChangeNick <$> get
     2 -> return Pong
     3 -> return ReJoin
     4 -> return Leave
     5 -> return Quit
     6 -> return NoAction
