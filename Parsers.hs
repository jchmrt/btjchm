{-# LANGUAGE OverloadedStrings #-}
module Parsers ( User
               , UserMessage(..)
               , IRCState(..)
               , MessageContext(..)
               , IRCParserState(..)
               , IRCAction(..)
               , parseMessage
               ) where

import Control.Applicative ((<$>))
import Control.Monad.State.Lazy
import Control.Monad.Identity
import Data.Time
import Text.Parsec
import qualified Data.Map as M
import qualified Data.Text as T

-- |A simple type synomym
type User = T.Text

-- |A data type representing a message
newtype UserMessage = UserMessage (T.Text, MessageContext) deriving Show

-- Should probably be in another module
-- |A data type representing the IRC State
data IRCState =
    IRCState { userMessages :: M.Map User [UserMessage] } deriving Show

data MessageContext =
    MessageContext { msgContextSenderNick :: T.Text
                   , msgContextSenderFull :: T.Text
                   , msgContextChannel :: T.Text
                   , msgContextTime :: UTCTime
                   } deriving Show

-- |A data type representing the state of the IRC Parser as wel as the
-- whole IRC State.
data IRCParserState =
    IRCParserState { ircState :: IRCState
                   , messageContext :: MessageContext
                   } deriving Show

-- |The IRCParser Monad, used for parsing messages. Incorporates the
-- State Monad and the Parsec Monad.
type IRCParser a = ParsecT T.Text ()
    (StateT IRCParserState Identity) a

-- |Run a IRCParser
runIRCParser :: IRCParser a -> SourceName -> T.Text
             -> IRCParserState -> Either ParseError (a,IRCParserState)
runIRCParser p s t st =
    case runIdentity $ runStateT (runParserT p () s t) st of
      (Left err, _) -> Left err
      (Right out, state) -> Right (out,state)

-- Should probably be in another module
-- |A data type representing an action the irc bot can take, intended
-- to be run by runAct.
data IRCAction = PrivMsg { privMsgText :: T.Text }
               | Pong
               | Leave
               | Quit
               | NoAction
    deriving Show

-- | Represents an message
data MessageType = PrivateMessage
                 | PingMessage
                 | OtherMessage

parseMessageType :: Parsec T.Text () MessageType
parseMessageType = do
  isPing <- parseWord
  case isPing of
    "PING" -> return PingMessage
    _      -> do 
      msgType <- parseWord
      case msgType of
          "PRIVMSG" -> return PrivateMessage
          "PING"    -> return PingMessage
          _         -> return OtherMessage

parseMessage :: T.Text -> IRCParserState -> (IRCAction, IRCParserState)
parseMessage str oldState =
  case runParser parseMessageType () "" str of
    Left _  -> (NoAction, oldState)
    Right t ->
      let parser = case t of
            PrivateMessage -> parsePrivateMessage
            PingMessage    -> return Pong
            OtherMessage   -> return NoAction
      in case runIRCParser parser "" str oldState of
            Left _            -> (NoAction, oldState)
            Right actAndState -> actAndState

parsePrivateMessage :: IRCParser IRCAction
parsePrivateMessage = do
  char ':'
  nick <- parseTill '!'
  putMsgContextSenderNick nick
  senderRest <- parseWord
  putMsgContextSenderFull (T.concat [nick,senderRest])
  parseWord
  chan <- parseWord
  putMsgContextChannel chan
  char ':'
  command <- parseWord
  case command of
    "!id"   -> parseCommandId
    "!tell" -> parseCommandTell
    _       -> return NoAction

parseCommandId :: IRCParser IRCAction
parseCommandId = do 
  text <- T.pack <$> many anyChar
  nick <- getMsgContextSenderNick
  chan <- getMsgContextChannel
  t    <- getMsgContextTime
  let time = T.pack $ show t
  return $ PrivMsg $ T.concat [nick, " said \"", text
                              ,"\" in ", chan, " at ", time]

parseCommandTell :: IRCParser IRCAction
parseCommandTell = do
  recipient <- parseWord
  msg <- fmap T.pack $ many anyChar

  cntxt <- getMessageContext
  let userMsg = UserMessage (msg, cntxt)

  userMsgs <- getUserMessages
  putUserMessages (M.insertWith (++) recipient [userMsg] userMsgs)
  return $ PrivMsg "I will tell it them, as soon as i see them"

parseWord :: Monad m => ParsecT T.Text u m T.Text
parseWord = parseTill ' '
parseTill :: Monad m => Char -> ParsecT T.Text u m T.Text
parseTill c = T.pack <$> manyTill anyChar (try (char c))

-- Getters, because I cant get ghci to work on arm -> so no TemplateHaskell,
-- which means I can't use lenses, and this is my workaround.
getIRCState :: IRCParser IRCState
getIRCState = gets ircState

getMessageContext :: IRCParser MessageContext
getMessageContext = gets messageContext

getUserMessages :: IRCParser (M.Map User [UserMessage])
getUserMessages = gets $ userMessages . ircState

getMsgContextSenderNick :: IRCParser T.Text
getMsgContextSenderNick = gets $ msgContextSenderNick . messageContext

getMsgContextSenderFull :: IRCParser T.Text
getMsgContextSenderFull = gets $ msgContextSenderFull . messageContext

getMsgContextChannel :: IRCParser T.Text
getMsgContextChannel = gets $ msgContextChannel . messageContext

getMsgContextTime :: IRCParser UTCTime
getMsgContextTime = gets $ msgContextTime . messageContext

-- Putters, same reason
putIRCState :: IRCState -> IRCParser ()
putIRCState new = do 
    old <- get
    put $ old { ircState = new }

putMessageContext :: MessageContext -> IRCParser ()
putMessageContext new = do 
    old <- get
    put $ old { messageContext = new }

putUserMessages :: M.Map User [UserMessage] -> IRCParser ()
putUserMessages new = do 
    old <- getIRCState
    putIRCState $ old { userMessages = new }

putMsgContextSenderNick :: T.Text -> IRCParser ()
putMsgContextSenderNick new = do
    old <- getMessageContext
    putMessageContext $ old { msgContextSenderNick = new }

putMsgContextSenderFull :: T.Text -> IRCParser ()
putMsgContextSenderFull new = do
    old <- getMessageContext
    putMessageContext $ old { msgContextSenderFull = new }

putMsgContextChannel :: T.Text -> IRCParser ()
putMsgContextChannel new = do
    old <- getMessageContext
    putMessageContext $ old { msgContextChannel = new }

putMsgContextTime :: UTCTime -> IRCParser ()
putMsgContextTime new = do
    old <- getMessageContext
    putMessageContext $ old { msgContextTime = new }
