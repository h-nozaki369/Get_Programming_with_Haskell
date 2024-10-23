{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as E
import Data.Maybe

type Author = T.Text
type Title = T.Text

data Book = Book { author :: Author, title :: Title } deriving Show

type Html = T.Text

bookToHtml :: Book -> Html
bookToHtml book = mconcat ["<p>\n",titleInTags,authorInTags,"</p>\n"]
    where titleInTags = mconcat ["<strong>",(title book),"</strong>\n"]
          authorInTags = mconcat ["<em>",(author book),"</em>\n"]

book1 :: Book
book1 = Book { title = "The Conspiracy Against the Human Race"
              ,author = "Ligotti, Thomas" }

book2 :: Book
book2 = Book { title = "A Short History of Decay"
              ,author = "Cioran, Emil" }

book3 :: Book
book3 = Book { title = "The Tears of Eros"
              ,author = "BatiIle, Georges" }

booksToHtml :: [Book] -> Html
booksToHtml books = mconcat ["<html>\n"
                            ,"<head><title>books</title>"
                            ,"<meta character='utf-8'/>"
                            ,"</head>\n"
                            ,"<body>\n"
                            ,booksHtml
                            ,"\n</body>\n"
                            ,"</html>"]
    where booksHtml = (mconcat . (map bookToHtml)) books

myBooks :: [Book]
myBooks = [book1,book2,book3]

main :: IO ()
main = TIO.writeFile "books.html" (booksToHtml myBooks)

type MarcRecordRaw = B.ByteString
type MarcLeaderRaw = B.ByteString

getLeader :: MarcRecordRaw -> MarcLeaderRaw
getLeader record = B.take leaderLength record

leaderLength :: Int
leaderLength = 24

rawToInt :: B.ByteString -> Int
rawToInt = (read . T.unpack . E.decodeUtf8)

getRecodeLength :: MarcLeaderRaw -> Int
getRecodeLength leader = rawToInt (B.take 5 leader)

nextAndRest :: B.ByteString -> (MarcRecordRaw, B.ByteString)
nextAndRest marcStream = B.splitAt recordLength marcStream
    where recordLength = getRecodeLength marcStream

allRecords :: B.ByteString -> [MarcRecordRaw]
allRecords marcStream = if marcStream == B.empty
                        then []
                        else next : allRecords rest
    where (next, rest) = nextAndRest marcStream
