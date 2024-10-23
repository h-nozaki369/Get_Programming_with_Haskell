allFmap :: Applicative f => (a -> b) -> f a -> f b
allFmap g x = (pure g) <*> x

main :: IO ()
main = do
    print $ allFmap (+1) [1,2,3]
    print $ allFmap (+1) (Just 5)
    print $ allFmap (+1) Nothing
