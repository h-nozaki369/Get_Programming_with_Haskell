data FiveSidedDie = S1 | S2 | S3 | S4 | S5

class (Eq a, Ord a) => Die a where
    value :: a -> Int

instance Die FiveSidedDie where
    -- value :: a -> Int
    value S1 = 0
    value S2 = 1
    value S3 = 2
    value S4 = 3
    value S5 = 4

instance Eq FiveSidedDie where
    -- (==) :: a -> a -> Bool
    x == y = value x == value y

instance Ord FiveSidedDie where
    -- compare :: a -> a -> Ordering
    compare x y = compare (value x) (value y)
