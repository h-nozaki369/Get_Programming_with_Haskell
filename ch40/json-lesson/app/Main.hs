module Main (main) where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import GHC.Generics

data Book = Book { title :: T.Text
                 , author :: T.Text
                 , year :: Int } deriving (Show, Generic)

instance FromJSON Book
instance ToJSON Book

myBook :: Book
myBook = Book {author = "Will Kurt", title = "Lear Haskell", year = 2017}

myBookJSON :: BC.ByteString
myBookJSON = encode myBook

rawJSON :: BC.ByteString
rawJSON = "{\"author\":\"Emil Ciroan\",\"title\":\"A Short History of Decay\",\"year\":1949}"

bookFromJSON :: Maybe Book
bookFromJSON = decode rawJSON

wrongJSON :: BC.ByteString
wrongJSON = "{\"writer\":\"Emil Ciroan\",\"title\":\"A Short History of Decay\",\"year\":1949}"

bookFromWrongJSON :: Maybe Book
bookFromWrongJSON = decode wrongJSON

sampleError :: BC.ByteString
sampleError = "{\"message\":\"oops!\",\"error\":123}"

data ErrorMessage = ErrorMessage { message :: T.Text
                                 , errorCode :: Int
                                 } deriving Show

instance FromJSON ErrorMessage where
    parseJSON (Object v) = ErrorMessage <$> v .: "message" <*> v .: "error"

exampleErrorMessage :: Maybe ErrorMessage
exampleErrorMessage = decode sampleError

instance ToJSON ErrorMessage where
    toJSON (ErrorMessage message errorCode) = object [ "message" .= message
                                                     , "error" .= errorCode
                                                     ]

anErrorMessage :: ErrorMessage
anErrorMessage = ErrorMessage "Everything is Okay" 0

main :: IO ()
main = print "hi"
