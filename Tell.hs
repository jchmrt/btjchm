{-# LANGUAGE OverloadedStrings #-}
module Tell (tellAll) where

import Core
import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Set as S

tellAll :: M.Map User (Maybe UserMessage) -> M.Map User [UserMessage] 
        -> ([IRCAction], M.Map User [UserMessage])
tellAll onlineUsers messages = M.foldlWithKey f ([],messages) onlineUsers
  where
    f acc          _    (Just _) = acc
    f (acts, msgs) user _        = 
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
tellMessages recipient usrMessages = 
    [PrivMsg $ T.concat [ "Hey ", recipient, " you have "
                        , tShow $ length usrMessages
                        , " messages:" ]]
    ++
    zipWith (curry makeMessage) [1..] $ reverse usrMessages
  where 
    makeMessage (n, UserMessage
      (txt, MessageContext nick _ chan time)) =
        PrivMsg $ T.concat [ tShow n, ": ", nick, " said \""
                           , txt, "\" in ", chan, " at "
                           , tShow time ]

tShow :: Show a => a -> T.Text
tShow = T.pack . show
