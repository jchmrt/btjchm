module TimedActions ( updateTimedActions )where

import Data.Time
import Core
import Data.List (foldl')

updateTimedActions :: UTCTime -> [(UTCTime, [IRCAction])] -> ([IRCAction],[(UTCTime, [IRCAction])])
updateTimedActions time = foldl' (\(accActs,accTimedActs) (t,acts) ->
                                   if t < time
                                   then (acts ++ accActs, accTimedActs)
                                   else (accActs, (t,acts):accTimedActs)) ([],[]) 
