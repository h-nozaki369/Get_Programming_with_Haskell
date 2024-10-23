import qualified Data.Map as Map
import System.Environment

data RobotPart = RobotPart { name :: String
                           , descrption :: String
                           , cost :: Double
                           , count :: Int } deriving Show

leftArm :: RobotPart
leftArm = RobotPart { name = "left arm"
                    , descrption = "left arm for face punching!"
                    , cost = 1000.00
                    , count = 3 }

rightArm :: RobotPart
rightArm = RobotPart { name = "right arm"
                     , descrption = "right arm for kind hand gestures"
                     , cost = 1025.00
                     , count = 5 }

robotHead :: RobotPart
robotHead = RobotPart { name = "robot head"
                      , descrption = "this head looks mad"
                      , cost = 6092.25
                      , count = 2 }

leftArm2 :: RobotPart
leftArm2 = RobotPart { name = "left arm 2"
                    , descrption = "left arm 2 for face punching!"
                    , cost = 1100.00
                    , count = 3 }

rightArm2 :: RobotPart
rightArm2 = RobotPart { name = "right arm 2"
                     , descrption = "right arm 2 for kind hand gestures"
                     , cost = 925.00
                     , count = 5 }

robotHead2 :: RobotPart
robotHead2 = RobotPart { name = "robot head 2"
                      , descrption = "robot head 2"
                      , cost = 7092.25
                      , count = 2 }

type Html = String

renderHtml :: RobotPart -> Html
renderHtml part = mconcat [ "<h2>", partName, "</h2>"
                          , "<p><h3>desc</h3>", partDesc
                          , "</p><p><h3>cost</h3>"
                          , partCost
                          , "</p><p><h3>count</h3>"
                          , partCount, "</p>" ]
    where partName = name part
          partDesc = descrption part
          partCost = show (cost part)
          partCount = show (count part)

partDB :: Map.Map Int RobotPart
partDB = Map.fromList keyVals
    where keys = [1,2,3,4,5,6]
          vals = [leftArm, rightArm, robotHead, leftArm2, rightArm2, robotHead2]
          keyVals = zip keys vals

minWith :: Ord b => (a -> b) -> a -> a -> a
minWith f x y = if (f x) < (f y) then x else y

printCheeperPart :: Maybe RobotPart -> IO ()
printCheeperPart Nothing = putStrLn "Error, invalid ID"
printCheeperPart (Just x) = putStrLn (show x)

main :: IO ()
main = do
    putStr "Enter ID 1: "
    id1 <- getLine
    putStr "Enter ID 2: "
    id2 <- getLine
    let part1 = Map.lookup (read id1) partDB 
    let part2 = Map.lookup (read id2) partDB 
    let cheeper = (minWith cost) <$> part1 <*> part2
    printCheeperPart cheeper
