import Data.Char

isPalindrome :: String -> Bool
isPalindrome xs = xs' == reverse xs'
    where xs' = map toLower $ filter isAlpha xs
