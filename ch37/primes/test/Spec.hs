import Test.QuickCheck
import Primes
import Data.Maybe

prop_validPrimesOnly val = if val < 2 || val > last primes
                           then result == Nothing
                           else isJust result
    where result = isPrime val

prop_primesArePrime val = if result == Just True
                          then length divisors == 0
                          else True
    where result = isPrime val
          divisors = filter ((==0) . (mod val)) [2 .. (val - 1)]

prop_nonPrimesAreComposite val = if result == Just False
                                 then length divisors > 0
                                 else True
    where result = isPrime val
          divisors = filter ((==0) . (mod val)) [2 .. (val - 1)]

prop_factorsMakeOriginal val = if result == Nothing
                               then True
                               else product (fromJust result) == val
    where result = primeFactors val

prop_allFactorsPrime val = if result == Nothing
                           then True
                           else all (== Just True) resultPrime
    where result = primeFactors val
          resultPrime = map isPrime (fromJust result)

main :: IO ()
main = do
    quickCheck prop_validPrimesOnly
    quickCheckWith stdArgs { maxSuccess = 10000 } prop_primesArePrime
    quickCheckWith stdArgs { maxSuccess = 10000 } prop_nonPrimesAreComposite
    quickCheckWith stdArgs { maxSuccess = 10000 } prop_factorsMakeOriginal
    quickCheckWith stdArgs { maxSuccess = 10000 } prop_allFactorsPrime
