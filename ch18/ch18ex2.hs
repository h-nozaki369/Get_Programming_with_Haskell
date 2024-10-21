import qualified Data.Map as Map

data Organ = Heart | Brain | Kedney | Spleen deriving (Show, Eq, Ord, Enum)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kedney]

ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

organInventory :: Map.Map Organ Int
organInventory = Map.fromList organNumbers
    where organNumbers = [(organ, numbers) | organ <- [Heart .. Spleen],
                                             let numbers = Map.size (Map.filter (== organ) organCatalog)]
