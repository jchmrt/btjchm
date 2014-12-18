{-# LANGUAGE OverloadedStrings #-}
module Parsers where

import Control.Monad.State.Lazy
import Control.Monad.Identity
import Data.Time
import Text.Parsec
import qualified Data.Map as M
import qualified Data.Text as T

-- |A simple type synomym
type User = T.Text

-- |A data type representing a message
data UserMessage =
    UserMessage { userMsgText   :: T.Text
                , userMsgSender :: User
                } deriving Show

-- Should probably be in another module
-- |A data type representing the IRC State
data IRCState =
    IRCState { userMessages :: M.Map User [UserMessage] } deriving Show

data MessageContext =
    MessageContext { msgContextSender :: T.Text
                   , msgContextTime :: UTCTime
                   } deriving Show

-- |A data type representing the state of the IRC Parser as wel as the
-- whole IRC State.
data IRCParserState =
    IRCParserState { ircState :: IRCState
                   , messageContext :: MessageContext
                   } deriving Show

-- Getters, because I cant get ghci to work on arm -> so no TemplateHaskell,
-- which means I can't use lenses, and this is my workaround.
getIRCState :: IRCParser IRCState
getIRCState = gets ircState

getMessageContext :: IRCParser MessageContext
getMessageContext = gets messageContext

getUserMessages :: IRCParser (M.Map User [UserMessage])
getUserMessages = gets $ userMessages . ircState

getMsgContextSender :: IRCParser T.Text
getMsgContextSender = gets $ msgContextSender . messageContext

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

putMsgContextSender :: T.Text -> IRCParser ()
putMsgContextSender new = do
    old <- getMessageContext
    putMessageContext $ old { msgContextSender = new }

putMsgContextTime :: UTCTime -> IRCParser ()
putMsgContextTime new = do
    old <- getMessageContext
    putMessageContext $ old { msgContextTime = new }

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
               | Leave
               | Quit
               | NoAction
    deriving Show

-- | Represents an message
data MessageType = PrivateMessage
                 | OtherMessage

parseMessageType :: Parsec T.Text () MessageType
parseMessageType = do
  parseWord
  msgType <- parseWord
  case msgType of
      "PRIVMSG" -> return PrivateMessage
      _         -> return OtherMessage

parseMessage :: T.Text -> IRCParserState -> (IRCAction, IRCParserState)
parseMessage str oldState =
  case runParser parseMessageType () "" str of
    Left _  -> (NoAction, oldState)
    Right t ->
      let parser = case t of
            PrivateMessage -> parsePrivateMessage
            OtherMessage   -> return NoAction
      in case runIRCParser parser "" str oldState of
            Left _            -> (NoAction, oldState)
            Right actAndState -> actAndState

parsePrivateMessage :: IRCParser IRCAction
parsePrivateMessage = do
  t <- getMsgContextTime
  let time = T.pack $ show t
  char ':'
  nick <- parseTill '!'
  parseWord
  parseWord
  chan <- parseWord
  char ':'
  command <- parseWord
  text <- fmap T.pack $ many anyChar
  case command of
    "!id" -> return $ PrivMsg $ T.concat [nick, " said \"", text
                                         ,"\" in ", chan, " at ", time]
    _     -> return $ NoAction

parseWord :: Monad m => ParsecT T.Text u m T.Text
parseWord = parseTill ' '
parseTill :: Monad m => Char -> ParsecT T.Text u m T.Text
parseTill c = fmap T.pack $ manyTill anyChar (try (char c))
