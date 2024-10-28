import Lib
import Data.Char (isPunctuation, isSpace)
import Test.QuickCheck
import Test.QuickCheck.Instances
import qualified Data.Text as T

prop_punctuationInvariant text = preprocess text == preprocess noPuncText
    where noPuncText = T.filter (not . isPunctuation) text

prop_reverseInvariant text = isPalindrome text == isPalindrome (T.reverse text)

prop_caseInvariant text = isPalindrome (T.toUpper text) == isPalindrome (T.toLower text)

prop_spaceRemovalInvariant text = isPalindrome text == isPalindrome (T.filter (not . isSpace) text)

prop_spaceAdditionInvariant text = isPalindrome text == isPalindrome (" \t " <> text <> " \n")

main :: IO ()
main = do
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_punctuationInvariant
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_reverseInvariant
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_caseInvariant
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_spaceRemovalInvariant
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_spaceAdditionInvariant
    putStrLn "done!"
