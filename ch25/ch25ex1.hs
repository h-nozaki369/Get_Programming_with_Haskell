import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as E
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    bytes <- B.readFile fileName
    let numBytes = B.length bytes
    let numChars = (T.length . E.decodeUtf8) bytes
    putStrLn $ mconcat ["File ",fileName," has ",show numChars," characters, and ",show numBytes," bytes."]
