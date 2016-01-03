import Core
import System.Random
import Save
import qualified Data.Map as M
import qualified Data.Text as T
import System.IO
import Data.Time

main = do
  g <- getStdGen
  zone <- getCurrentTimeZone
  let new = IRCState M.empty M.empty [] (T.pack "") g zone
  save "state.sav" new
