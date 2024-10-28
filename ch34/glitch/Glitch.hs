module Glitch (randomReplaceByte
              ,randomSortSection
              ,randomReverseBytes
              ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.Random

intToChar :: Int -> Char
intToChar int = toEnum safeInt
    where safeInt = int `mod` 256 

intToBC :: Int -> BC.ByteString
intToBC int = BC.pack [intToChar int]

replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte loc charVal bytes = mconcat [before, newChar, after]
    where (before, reset) = BC.splitAt loc bytes
          after = BC.drop 1 reset
          newChar = intToBC charVal

randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
    let bytesLength = BC.length bytes
    location <- randomRIO (1, bytesLength)
    charVal <- randomRIO (0, 255)
    return (replaceByte location charVal bytes)

sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size bytes = mconcat [before, changed, after]
    where (before,rest) = BC.splitAt start bytes
          (target,after) = BC.splitAt size rest
          changed = BC.reverse (BC.sort target)

randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection bytes = do
    let sectionSize = 25
    let bytesLength = BC.length bytes
    start <- randomRIO (0, bytesLength - sectionSize)
    return (sortSection start sectionSize bytes)

reverseBytes :: Int -> Int -> BC.ByteString -> BC.ByteString
reverseBytes start size bytes = mconcat [before, changed, after]
    where (before,rest) = BC.splitAt start bytes
          (target,after) = BC.splitAt size rest
          changed = BC.reverse (BC.reverse target)

randomReverseBytes :: BC.ByteString -> IO BC.ByteString
randomReverseBytes bytes = do
    let sectionSize = 25
    let bytesLength = BC.length bytes
    start <- randomRIO (0, bytesLength - sectionSize)
    return (reverseBytes start sectionSize bytes)
