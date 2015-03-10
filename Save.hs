module Save ( save
            , retrieve 
            ) where

import Core
import Binary
import Data.Binary
import qualified Data.ByteString.Lazy as L
import Control.Applicative ((<$>))

save :: FilePath -> IRCState -> IO ()
save fp = L.writeFile fp . encode

retrieve :: FilePath -> IO IRCState
retrieve fp = decode <$> L.readFile fp
