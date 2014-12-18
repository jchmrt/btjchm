{-# LANGUAGE OverloadedStrings #-}
module Main where

import Parsers
import Network
import System.IO
import Text.Printf
import Data.List
import Data.Time
import qualified Data.Map as M
import System.Exit
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Parsec
import Control.Monad.Reader

server  = "irc.freenode.org"
port    = 6667
chan    = T.pack "#eras"
nick    = T.pack "btjchm"
user    = T.pack " 0 * :jchmrt's bot"
usermsg = T.concat [nick,user]

main :: IO ()
main = do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    write h (T.pack "NICK") nick
    write h (T.pack "USER") usermsg
    write h (T.pack "JOIN") chan
    listen h

write :: Handle -> T.Text -> T.Text -> IO ()
write h s t = do
    TIO.hPutStr h $ T.concat [s, T.pack " ", t, T.pack "\r\n"]
    TIO.putStrLn $ T.concat [T.pack ">  ", s, T.pack " ", t]

listen :: Handle -> IO ()
listen h = forever $ do
    t <- TIO.hGetLine h
    let s = T.init t
    if ping s then pong s else eval h s
    TIO.putStrLn s
  where
    forever a = a >> forever a
    ping x = T.pack "PING :" `T.isPrefixOf` x
    pong x = write h (T.pack "PONG") $ T.concat [T.pack ":", T.drop 6 x]

eval :: Handle -> T.Text -> IO ()
eval h s = do
  time <- getCurrentTime
  let parserState = IRCParserState (IRCState M.empty)
                                   (MessageContext "" time)
  runAct h $ fst $ parseMessage s parserState
                          
defaultIRCParserState = IRCParserState
                        (IRCState M.empty)
                        (MessageContext undefined undefined)

runAct :: Handle -> IRCAction -> IO ()
runAct h (PrivMsg t) = privmsg h t
runAct _ _           = return ()

privmsg :: Handle -> T.Text -> IO ()
privmsg h s = write h (T.pack "PRIVMSG ") $ T.concat [chan,T.pack " :",s]
