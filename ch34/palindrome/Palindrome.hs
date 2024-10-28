module Palindrome (isPalindrome) where

import Data.Char (isSpace, isPunctuation)
import Data.Text as T (Text, toLower, filter, reverse)

stripWhiteSpace :: T.Text -> T.Text
stripWhiteSpace = T.filter (not . isSpace)

stripPunctuation :: T.Text -> T.Text
stripPunctuation = T.filter (not. isPunctuation)

toLowerCase :: T.Text -> T.Text
toLowerCase = T.toLower

preprocess :: T.Text -> T.Text
preprocess = stripWhiteSpace . stripPunctuation . toLowerCase

isPalindrome :: T.Text -> Bool
isPalindrome text = cleanText == T.reverse cleanText
    where cleanText = preprocess text
