import Core
import System.Random
import Save
import qualified Data.Map as M
import qualified Data.Text as T
import System.IO

main = do
  g <- getStdGen
  let new = IRCState M.empty M.empty [] (T.pack "") g
  save "state.sav" new
