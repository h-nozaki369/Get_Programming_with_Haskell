data Box a = Box a deriving Show

instance Functor Box where
    -- fmap :: (a -> b) -> Box a -> Box b
    fmap f (Box x) = Box (f x)

morePresents :: Int -> Box a -> Box [a]
morePresents n = fmap (replicate n)

myBox :: Box Int
myBox = Box 1

wrapped = fmap Box myBox

unwrap :: Box a -> Box a
unwrap (Box x) = x
