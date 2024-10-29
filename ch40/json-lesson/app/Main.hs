module Main (main) where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import GHC.Generics
import Control.Monad

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

data NOAAResult = NOAAResult { uid :: T.Text
                             , mindate :: T.Text
                             , maxdate :: T.Text
                             , name :: T.Text
                             , datacoverage :: Double
                             , resultId :: T.Text } deriving Show

instance FromJSON NOAAResult where
    parseJSON (Object v) = NOAAResult <$> v .: "uid"
                                      <*> v .: "mindate"
                                      <*> v .: "maxdate"
                                      <*> v .: "name"
                                      <*> v .: "datacoverage"
                                      <*> v .: "id"

-- Q40-1
instance ToJSON NOAAResult where
    toJSON (NOAAResult uid mindate maxdate name datacoverage resultId) =
        object [ "uid" .= uid
               , "mindate" .= mindate
               , "maxdate" .= maxdate
               , "name" .= name
               , "datacoverage" .= datacoverage
               , "id" .= resultId
               ]

data Resultset = Resultset { offset :: Int
                           , count :: Int
                           , limit :: Int } deriving (Show, Generic)

instance FromJSON Resultset
-- Q40-1
instance ToJSON Resultset

data Metadata = Metadata { resultset :: Resultset } deriving (Show, Generic)

instance FromJSON Metadata
-- Q40-1
instance ToJSON Metadata

data NOAAResponse = NOAAResponse { metadata :: Metadata
                                 , results :: [NOAAResult]
                                 } deriving (Show, Generic)

instance FromJSON NOAAResponse
-- Q40-1
instance ToJSON NOAAResponse

printResults :: Maybe [NOAAResult] -> IO ()
printResults Nothing = print "error loading data"
printResults (Just results) = do
    forM_ results $ \result -> do
        let dataName = name result
        print dataName

-- Q40-2
data IntList = EmptyList | Cons Int IntList deriving (Show, Generic)

instance FromJSON IntList
instance ToJSON IntList

intListExample :: IntList
intListExample = Cons 1 $ Cons 2 EmptyList

main :: IO ()
main = do
    jsonData <- B.readFile "data.json"
    let noaaResponse = decode jsonData :: Maybe NOAAResponse
    let noaaResults = results <$> noaaResponse
    printResults noaaResults
