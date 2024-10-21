main :: IO ()
main = do
    userInput <- getContents
    mapM_ (putStrLn . showProverb) (lines userInput)

showProverb :: String -> String
showProverb ans = case ans of
                      "n" -> undefined
                      "N" -> undefined
                      "1" -> "Proverb 1"
                      "2" -> "Proverb 2"
                      "3" -> "Proverb 3"
                      "4" -> "Proverb 4"
                      "5" -> "Proverb 5"
                      otherwise -> undefined
                           
