import System.Environment

calcArgs :: [String] -> String
calcArgs args =
    let n1 = read (args !! 0)
        n2 = read (args !! 2)
    in case args !! 1 of
           "+" -> show (n1 + n2)
           "*" -> show (n1 * n2)
           otherwise -> "Invalid operator"

main :: IO ()
main = do
    args <- getArgs
    let result = calcArgs args
    putStrLn result
