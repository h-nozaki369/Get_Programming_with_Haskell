allApp :: Monad m => m (a -> b) -> m a -> m b
allApp f x = f >>= (\g -> x >>= (\y -> (return . g) y))
