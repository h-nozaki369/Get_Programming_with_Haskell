data FiveSidedDie = S1 | S2 | S3 | S4 | S5 deriving (Enum)

instance Eq FiveSidedDie where
    -- (==) :: a -> a -> Bool
    x == y = fromEnum x == fromEnum y

instance Ord FiveSidedDie where
    -- compare :: a -> a -> Ordering
    compare x y = compare (fromEnum x) (fromEnum y)
