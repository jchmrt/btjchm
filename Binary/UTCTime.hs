module Binary.UTCTime where

import Data.Binary
import Data.Time

instance Binary Day where
  put day = put $ toModifiedJulianDay day
  get = do d <- get
           return $ ModifiedJulianDay d

instance Binary DiffTime where
  put t = put (toRational t)
  get = do t <- get
           return $ fromRational t

instance Binary UTCTime where
  put (UTCTime day time) = put day >> put time
  get = do day <- get
           time <- get
           return $ UTCTime day time
