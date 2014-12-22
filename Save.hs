module Save ( writeUserMessagesToFile
            , readUserMessagesFromFile 
            ) where

import Core
import qualified Data.Text as T
import qualified Data.Map as M
import Control.Applicative ((<$>))

writeUserMessagesToFile :: FilePath -> M.Map User [UserMessage] -> IO ()
writeUserMessagesToFile fp usrMessages = writeFile fp $ show usrMessages

readUserMessagesFromFile :: FilePath -> IO (M.Map User [UserMessage])
readUserMessagesFromFile fp = read <$> readFile fp
