allFmapM :: Monad m => (a -> b) -> m a -> m b
allFmapM f x = x >>= (\y -> (return . f) y)
