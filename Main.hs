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
    let emptyState = (IRCState M.empty)
    listen h emptyState

write :: Handle -> T.Text -> T.Text -> IO ()
write h s t = do
    TIO.hPutStr h $ T.concat [s, T.pack " ", t, T.pack "\r\n"]
    TIO.putStrLn $ T.concat [T.pack ">  ", s, T.pack " ", t]

listen :: Handle -> IRCState -> IO ()
listen h st = do
    t <- TIO.hGetLine h
    let s = T.init t
    nst <- eval h s st
    TIO.putStrLn s
    listen h nst

eval :: Handle -> T.Text -> IRCState -> IO IRCState
eval h s ircState = do
  time <- getCurrentTime
  let parserState = IRCParserState ircState (MessageContext "" "" "" time) 
      (act, (IRCParserState newIrcState _)) = parseMessage s parserState
  runAct h act
  return newIrcState

runAct :: Handle -> IRCAction -> IO ()
runAct h (PrivMsg t) = privmsg h t
runAct _ _           = return ()

privmsg :: Handle -> T.Text -> IO ()
privmsg h s = write h (T.pack "PRIVMSG ") $ T.concat [chan,T.pack " :",s]
