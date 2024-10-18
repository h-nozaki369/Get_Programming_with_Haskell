data Events = Events [String]
data Probs = Probs [Double]
data PTable = PTable Events Probs

createTable :: Events -> Probs -> PTable
createTable (Events events) (Probs probs) = PTable (Events events) (Probs normalizedProbs)
    where totalProbs = sum probs
          normalizedProbs = map (/ totalProbs) probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

instance Show PTable where
    show (PTable (Events events) (Probs probs)) = mconcat pairs
        where pairs = zipWith showPair events probs

instance Semigroup Events where
    e1 <> e2 = cartCombine combinar e1 e2
        where combinar = (\x y -> mconcat [x,"-",y])

instance Semigroup Probs where
    p1 <> p2 = cartCombine (*) p1 p2

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
    where nToAdd = length l2
          repeatedL1 = map (take nToAdd . repeat) l1
          newL1 = mconcat repeatedL1
          cycledL2 = cycle l2
