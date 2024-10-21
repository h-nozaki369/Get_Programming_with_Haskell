import qualified Data.Map as Map

helloPerson :: String -> String
helloPerson name = "Hello" ++ " " ++ name ++ "!"

maybeMain :: Maybe String
maybeMain = do
    name <- Map.lookup 1 nameData
    let statement = helloPerson name
    return statement

nameData :: Map.Map Int String
nameData = Map.fromList [(1, "Bob"), (2, "Alice")]
