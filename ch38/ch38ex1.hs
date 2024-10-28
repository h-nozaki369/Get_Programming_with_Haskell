import Data.Char (isDigit)

addStrInts :: String -> String -> Either String Int
addStrInts xs ys = case (validXs, validYs) of
                    (True,  True)  -> Right (read xs + read ys)
                    (False, True)  -> Left "First Argument Invalid "
                    (True,  False) -> Left "Second Argument Invalid "
                    (False, False) -> Left "Both Arguments Invalid "
    where isDigits = all isDigit 
          validXs = isDigits xs
          validYs = isDigits ys
