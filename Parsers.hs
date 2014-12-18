{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Parsers where

import Control.Lens
import Control.Monad.State.Lazy
import Data.Time
import Text.Parsec
import qualified Data.Map as M
import qualified Data.Text as T

-- |A simple type synomym
type User = T.Text

-- |A data type representing a message
data UserMessage =
    UserMessage { _userMsgText   :: T.Text
                , _userMsgSender :: User
                } deriving Show
makeLenses ''UserMessage

-- Should probably be in another module
-- |A data type representing the IRC State
data IRCState =
    IRCState { _messages :: M.Map User [UserMessage] } deriving Show
makeLenses ''IRCState

data MessageContext =
    MessageContext { _msgContextSender :: T.Text
                   , _msgContextTime :: UTCTime
                   } deriving Show
makeLenses ''MessageContext

-- |A data type representing the state of the IRC Parser as wel as the
-- whole IRC State.
data IRCParserState =
    IRCParserState { _ircState :: IRCState
                   , _messageContext :: MessageContext
                   } deriving Show
makeLenses ''IRCParserState

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
  st <- get
  let time = T.pack $ show 
             $ view (messageContext . msgContextTime) st
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

parseWord :: Monad m => ParsecT T.Text u m T.Text
parseWord = parseTill ' '
parseTill :: Monad m => Char -> ParsecT T.Text u m T.Text
parseTill c = fmap T.pack $ manyTill anyChar (try (char c))
