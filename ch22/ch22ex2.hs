main :: IO ()
main = do
    userInput <- getContents
    mapM_ putStrLn (showProverb (lines userInput))

showProverb :: [String] -> [String]
showProverb [] = []
showProverb ("n":_) = []
showProverb (x:xs) = (proverbs !! (read x - 1)) : showProverb xs
                           
proverbs :: [String]
proverbs = ["Proverb 1", "Proverb 2", "Proverb 3", "Proverb 4", "Proverb 5"]
