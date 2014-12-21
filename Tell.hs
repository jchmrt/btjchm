{-# LANGUAGE OverloadedStrings #-}
module Tell (tellAll) where

-- It's not ideal that we have to import Parsers here, but it is the
-- best we can do right now. FIX ME
import Parsers
import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Set as S

tellAll :: S.Set User -> M.Map User [UserMessage] 
        -> ([IRCAction], M.Map User [UserMessage])
tellAll onlineUsers messages = S.foldl' f ([],messages) onlineUsers
  where f (acts, msgs) user = 
            let (newActs, newMessages) = tell user msgs
            in (newActs++acts, newMessages)
               
tell :: User -> M.Map User [UserMessage] 
     -> ([IRCAction], M.Map User [UserMessage])
tell recipient usrMessages = 
    case M.lookup recipient usrMessages of
      Just msgs -> ( tellMessages recipient msgs
                   , M.delete recipient usrMessages)
      Nothing   -> ([], usrMessages)

tellMessages :: User -> [UserMessage] -> [IRCAction]
tellMessages recipient usrMessages = concat
    [ [PrivMsg $ T.concat [ "Hey ", recipient, " you have "
                          , tShow $ length usrMessages
                          , " messages:" ]]
    , zipWith (curry makeMessage) [1..] usrMessages ]
  where 
    makeMessage (n, UserMessage (txt 
      ,(MessageContext nick full chan time))) =
        PrivMsg $ T.concat [ tShow n, ": ", nick, " said \""
                           , txt, "\" in ", chan, " at "
                           , tShow time ]

tShow :: Show a => a -> T.Text
tShow = T.pack . show
