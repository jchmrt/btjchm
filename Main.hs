{-# LANGUAGE OverloadedStrings #-}
module Main where

import Core
import Parsers
import Tell
import Save
import TimedActions
import Network
import System.IO
import Data.Time
import Control.Monad
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Text.IO as TIO

server :: String
server  = "irc.freenode.org"
port :: Int
port    = 6667
chan,nick,user,usermsg :: T.Text
chan    = T.pack "#eras"
nick    = T.pack "btjchm"
user    = T.pack " 0 * :jchmrt's bot"
usermsg = T.concat [nick,user]

saveFile = "state.sav"

main :: IO ()
main = do
  h <- connectTo server (PortNumber (fromIntegral port))
  hSetBuffering h NoBuffering
  write h (T.pack "NICK") nick
  write h (T.pack "USER") usermsg
  write h (T.pack "JOIN") chan
  (IRCState msgs _ acts) <- retrieve saveFile
  let newState = IRCState msgs M.empty acts
  listen h newState

write :: Handle -> T.Text -> T.Text -> IO ()
write h s t = do
  TIO.hPutStr h $ T.concat [s, T.pack " ", t, T.pack "\r\n"]
  TIO.putStrLn $ T.concat [T.pack ">  ", s, T.pack " ", t]

listen :: Handle -> IRCState -> IO ()
listen h st = do
  inputAvailable <- hWaitForInput h 1000
  nst <- (if inputAvailable
          then (do
                   t <- TIO.hGetLine h
                   let s = T.init t
                   nst <- eval h s st
                   TIO.putStrLn s
                   return nst)

          else return st)
  time <- getCurrentTime

  let (IRCState usrMessages usrs timedActs) = nst
      (IRCState oldUsrMessages _ oldTimedActs) = st
      (tellActs, newUsrMessages) = tellAll usrs usrMessages

      (timedActsToRun,newTimedActs) = updateTimedActions time timedActs
      nst' = IRCState newUsrMessages usrs newTimedActs

  runActs h tellActs
  runActs h timedActsToRun

  when (newUsrMessages /= oldUsrMessages
     || newTimedActs /= oldTimedActs)
    (save saveFile
        (IRCState newUsrMessages M.empty newTimedActs) >> putStrLn "-- Saving --")
  listen h nst'

eval :: Handle -> T.Text -> IRCState -> IO IRCState
eval h s ircSt = do
  time <- getCurrentTime
  let parserState = IRCParserState ircSt (MessageContext "" "" "" time) 
      (act, IRCParserState newIrcState _) = parseMessage s parserState
  case act of
    [Pong] -> pong h s
    [Debug] -> print ircSt
    _    -> runActs h act
  return newIrcState

runActs :: Handle -> [IRCAction] -> IO ()
runActs h = mapM_ (runAct h) 

runAct :: Handle -> IRCAction -> IO ()
runAct h (PrivMsg t) = privmsg h t
runAct h ReJoin      = write h (T.pack "JOIN") chan
runAct _ _           = return ()

privmsg :: Handle -> T.Text -> IO ()
privmsg h s = write h (T.pack "PRIVMSG ") $ T.concat [chan,T.pack " :",s]

pong :: Handle -> T.Text -> IO ()
pong h s = write h (T.pack "PONG") $ T.concat [":",T.drop 6 s]
