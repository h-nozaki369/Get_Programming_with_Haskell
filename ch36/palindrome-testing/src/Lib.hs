module Lib
    ( isPalindrome
    , preprocess
    ) where

import Data.Char (isPunctuation, isSpace)
import qualified Data.Text as T

preprocess :: T.Text -> T.Text
preprocess = (T.filter (not . isPunctuation)) . (T.filter (not . isSpace)) . T.toLower

isPalindrome :: T.Text -> Bool
isPalindrome text = cleanText == T.reverse cleanText
    where cleanText = preprocess text
