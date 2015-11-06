module Core ( User
            , UserMessage (..)
            , IRCState (..)
            , MessageContext (..)
            , IRCAction (..)
            ) where
 
import System.Random
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time

-- |A simple type synomym
type User = T.Text

-- |A data type representing a message
newtype UserMessage =
  UserMessage (T.Text, MessageContext) deriving (Show,Read,Eq)

-- |A data type representing the IRC State
data IRCState = IRCState
  { userMessages :: M.Map User [UserMessage]
    -- ^ The !tell messages.
  , onlineUsers  :: M.Map User (Maybe UserMessage)
    -- ^ The online users, with a UserMessage if they are afk,
    -- otherwise nothing
  , timedActions :: [(UTCTime, [IRCAction])]
    -- ^ The actions to execute at a certain time.
  , key :: T.Text
    -- ^ The key of this bot process
  , randomGen :: StdGen
    -- ^ The seed used to create random values
  , timeZone :: TimeZone
    -- ^ The current time zone
  } deriving Show

data MessageContext =
    MessageContext { msgContextSenderNick :: T.Text
                   , msgContextSenderFull :: T.Text
                   , msgContextChannel    :: T.Text
                   , msgContextTime       :: UTCTime
                   } deriving (Show,Read,Eq)

-- |A data type representing an action the irc bot can take, intended
-- to be run by runAct in Main.
data IRCAction = PrivMsg { privMsgText :: T.Text }
               | ChangeNick { nick :: T.Text }
               | Debug { debugText :: T.Text }
               | Pong
               | ReJoin
               | Leave
               | Quit
               | NoAction
    deriving (Show, Eq)
