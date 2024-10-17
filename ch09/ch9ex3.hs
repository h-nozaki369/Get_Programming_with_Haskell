harmonic :: Int -> Double
harmonic n = sum $ take n $ map ((1.0 /) . fromIntegral) [1..]
