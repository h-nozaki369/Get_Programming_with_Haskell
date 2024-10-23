import System.IO
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
   args <- getArgs
   let inputFile = head args
   let outputFile = args !! 1
   input <- TIO.readFile inputFile
   TIO.writeFile outputFile input
