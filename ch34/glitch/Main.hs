module Main where

import Glitch
import System.Environment
import qualified Data.ByteString.Char8 as BC
import Control.Monad

main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    imageFile <- BC.readFile fileName
    glitched <- foldM (\bytes func -> func bytes) imageFile
                                                  [randomReplaceByte
                                                  ,randomSortSection
                                                  ,randomReverseBytes
                                                  ,randomReplaceByte
                                                  ,randomSortSection
                                                  ,randomReverseBytes
                                                  ,randomReplaceByte]
    let glitchedFileName = mconcat ["glitched_",fileName]
    BC.writeFile glitchedFileName glitched
    print "all done"

