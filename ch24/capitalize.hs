import System.IO
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
   args <- getArgs
   let inoutFile = head args
   input <- TIO.readFile inoutFile
   let output = T.toUpper input
   TIO.writeFile inoutFile output
