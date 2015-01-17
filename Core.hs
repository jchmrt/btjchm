module Core ( User
            , UserMessage (..)
            , IRCState (..)
            , MessageContext (..)
            , IRCAction (..)
            ) where
 
import Control.Monad.State
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time

-- |A simple type synomym
type User = T.Text

-- |A data type representing a message
newtype UserMessage = UserMessage (T.Text, MessageContext) deriving (Show,Read,Eq)

-- |A data type representing the IRC State
data IRCState =
    IRCState { userMessages :: M.Map User [UserMessage] 
             , onlineUsers  :: M.Map User (Maybe UserMessage)
             , timedActions :: [(UTCTime, [IRCAction])]
             } deriving Show

data MessageContext =
    MessageContext { msgContextSenderNick :: T.Text
                   , msgContextSenderFull :: T.Text
                   , msgContextChannel    :: T.Text
                   , msgContextTime       :: UTCTime
                   } deriving (Show,Read,Eq)

-- |A data type representing an action the irc bot can take, intended
-- to be run by runAct.
data IRCAction = PrivMsg { privMsgText :: T.Text }
               | Pong
               | ReJoin
               | Leave
               | Quit
               | NoAction
    deriving Show
