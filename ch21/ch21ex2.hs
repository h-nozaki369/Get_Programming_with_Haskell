fastFib :: Integer -> Integer -> Integer -> Integer
fastFib _  _  0       = 0
fastFib n1 n2 1       = n1
fastFib n1 n2 2       = n2
fastFib n1 n2 counter = fastFib n2 (n1+n2) (counter-1)

fib :: Integer -> Integer
fib n = fastFib 1 1 n

main :: IO ()
main = do
    putStr "What number of Fibonacci numbers do you want to display? "
    n <- getLine
    let fibNum = fib (read n)
    putStrLn (show fibNum)
