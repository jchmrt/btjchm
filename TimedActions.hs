module TimedActions ( updateTimedActions )where

import Data.Time
import Parsers
import Data.List (foldl')

updateTimedActions :: UTCTime -> [(UTCTime, [IRCAction])] -> ([IRCAction],[(UTCTime, [IRCAction])])
updateTimedActions time timedActs = foldl' (\(accActs,accTimedActs) (t,acts) -> if t < time
                                                                                then (acts ++ accActs, accTimedActs)
                                                                                else (accActs, (t,acts):accTimedActs))
                                           ([],[]) timedActs
