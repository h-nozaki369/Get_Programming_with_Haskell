data Box a = Box a deriving Show
boxMap :: (a -> a) -> Box a -> Box a
boxMap f (Box x) = Box (f x)

data Triple a = Triple a a a deriving Show
tripleMap :: (a -> a) -> Triple a -> Triple a
tripleMap f (Triple x y z) = Triple (f x) (f y) (f z)
