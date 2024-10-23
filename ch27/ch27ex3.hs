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
    where keys = [1,2,3]
          vals = [leftArm, rightArm, robotHead]
          keyVals = zip keys vals

getPartCost :: Map.Map Int RobotPart -> Int -> Maybe Double
getPartCost db id = cost <$> Map.lookup id db

main :: IO ()
main = do
    args <- getArgs
    let id = (read . head) args
    case getPartCost partDB id of
        Nothing -> putStrLn "ID not found"
        Just x -> putStrLn $ "Cost: " ++ show x
