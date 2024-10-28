safeSucc :: (Eq a, Enum a, Bounded a) => a -> Maybe a
safeSucc n | n == maxBound = Nothing
           | otherwise = Just (succ n)

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x:xs) = xs

safeLast :: [a] -> Either String a
safeLast [] = Left "Empty list"
safeLast xs = safeLast' 10000 xs
    where safeLast' 0 _ = Left "Exceed max bound"
          safeLast' _ [x] = Right x
          safeLast' n (x:xs) = safeLast' (n-1) xs
