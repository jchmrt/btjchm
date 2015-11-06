module Save ( save
            , retrieve 
            ) where

import Core
import Binary
import Data.Binary
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Control.Applicative ((<$>))

save :: FilePath -> IRCState -> IO ()
save fp = B.writeFile fp . L.toStrict . encode

retrieve :: FilePath -> IO IRCState
retrieve fp = decode . L.fromStrict <$> B.readFile fp
