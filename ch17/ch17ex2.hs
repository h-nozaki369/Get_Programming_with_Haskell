data Events = Events [String] deriving (Show, Eq)
data Probs = Probs [Double] deriving (Show, Eq)
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
    Events e1 <> Events e2 = Events $ cartCombine combinar e1 e2
        where combinar = (\x y -> mconcat [x,"-",y])

instance Semigroup Probs where
    Probs p1 <> Probs p2 = Probs $ cartCombine (*) p1 p2

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
    where nToAdd = length l2
          repeatedL1 = map (take nToAdd . repeat) l1
          newL1 = mconcat repeatedL1
          cycledL2 = cycle l2

instance Monoid Events where
    mempty = Events []
    mappend = (<>)

instance Monoid Probs where
    mempty = Probs []
    mappend = (<>)

instance Semigroup PTable where
    (<>) ptable1 (PTable (Events []) (Probs [])) = ptable1
    (<>) (PTable (Events []) (Probs [])) ptable2 = ptable2
    (<>) (PTable e1 p1) (PTable e2 p2) = createTable newEvents newProbs
        where newEvents = e1 <> e2
              newProbs = p1 <> p2

instance Monoid PTable where
    mempty = PTable (Events []) (Probs [])
    mappend = (<>)
