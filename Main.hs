{-# LANGUAGE OverloadedStrings #-}
module Main where

import Core
import Parsers
import Tell
import Save
import TimedActions
import Network
import System.IO
import System.Random
import System.Environment
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
chan,stdNick,user,usermsg :: T.Text
chan    = "#eras"
stdNick = "btjchm"
user    = " 0 * :jchmrt's bot"
usermsg = T.concat [stdNick,user]

saveFile = "state.sav"

main :: IO ()
main = do
  args <- getArgs
  let nick = case args of
        [argNick] -> T.pack argNick
        _ -> stdNick
  h <- connectTo server (PortNumber (fromIntegral port))
  hSetBuffering h NoBuffering
  write h (T.pack "NICK") nick
  write h (T.pack "USER") usermsg
  write h (T.pack "JOIN") chan
  (IRCState msgs _ acts _ gen _) <- retrieve saveFile
  g <- newStdGen
  zone <- getCurrentTimeZone
  let key = T.pack $ take 4 $ randomRs ('a','z') g
      newState = IRCState msgs M.empty acts key gen zone
  listen h newState

write :: Handle -> T.Text -> T.Text -> IO ()
write h s t = do
  TIO.hPutStr h $ T.concat [s, " ", t, "\r\n"]
  TIO.putStrLn $ T.concat [">  ", s, " ", t]

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

  let (IRCState oldUsrMessages _ oldTimedActs key oldGen zone) = st
      (IRCState usrMessages usrs timedActs _ newGen _) = nst
      (tellActs, newUsrMessages) = tellAll usrs usrMessages

      (timedActsToRun,newTimedActs) = updateTimedActions time timedActs
      nst' = IRCState newUsrMessages usrs newTimedActs key newGen zone

  runActs h tellActs
  runActs h timedActsToRun

  if (newUsrMessages /= oldUsrMessages
     || newTimedActs /= oldTimedActs
     || show oldGen /= show newGen)
    then do save saveFile $ IRCState newUsrMessages M.empty
                                     newTimedActs T.empty newGen zone
            putStrLn "-- Saving --"
    else return ()
  listen h nst'

eval :: Handle -> T.Text -> IRCState -> IO IRCState
eval h s ircSt = do
  time <- getCurrentTime
  let parserState = IRCParserState ircSt (MessageContext "" "" "" time) 
      (act, IRCParserState newIrcState _) = parseMessage s parserState
  case act of
    [Pong] -> pong h s
    _    -> runActs h act
  return newIrcState

runActs :: Handle -> [IRCAction] -> IO ()
runActs h = mapM_ (runAct h) 

runAct :: Handle -> IRCAction -> IO ()
runAct h (PrivMsg t)    = privmsg h t
runAct h ReJoin         = write h "JOIN" chan
runAct h (Debug msg)    = TIO.putStrLn msg
runAct h (ChangeNick n) = changeNick h n
runAct _ _              = return ()

privmsg :: Handle -> T.Text -> IO ()
privmsg h s = write h "PRIVMSG " $ T.concat [chan," :",s]

pong :: Handle -> T.Text -> IO ()
pong h s = write h "PONG" $ T.concat [":",T.drop 6 s]

changeNick :: Handle -> T.Text -> IO ()
changeNick h new = write h "NICK" new
