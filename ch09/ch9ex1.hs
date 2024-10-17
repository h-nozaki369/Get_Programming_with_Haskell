elem' :: Eq a => a -> [a] -> Bool
elem' x xs = length (filter (==x) xs) > 0
