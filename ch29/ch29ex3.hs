boughtBears :: [Int]
boughtBears = [6, 12]

drunkBears :: [Int]
drunkBears = [2 * 2]

remainingBears :: [Int]
remainingBears = (-) <$> boughtBears <*> drunkBears

friends :: [Int]
friends = [2, 3]

bearsPerPerson :: [Int]
bearsPerPerson = [3, 4]

necessaryBears :: [Int]
necessaryBears = (*) <$> ((+) <$> friends <*> [2]) <*> bearsPerPerson

needToBuy :: Int
needToBuy = maximum necessaryBears - minimum remainingBears
