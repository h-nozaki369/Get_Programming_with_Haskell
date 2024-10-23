calendar :: [String]
calendar = do
    (month, days) <- [ ("Jan ",31)
                     , ("Feb ",28)
                     , ("Mar ",31)
                     , ("Apr ",30)
                     , ("May ",31)
                     , ("Jun ",30)
                     , ("Jul ",31)
                     , ("Aug ",31)
                     , ("Sep ",30)
                     , ("Oct ",31)
                     , ("Nov ",30)
                     , ("Dec ",31) ]
    day <- [1 .. days]
    return (month ++ show day)

calendar' :: [String]
calendar' =
    [ ("Jan ",31)
    , ("Feb ",28)
    , ("Mar ",31)
    , ("Apr ",30)
    , ("May ",31)
    , ("Jun ",30)
    , ("Jul ",31)
    , ("Aug ",31)
    , ("Sep ",30)
    , ("Oct ",31)
    , ("Nov ",30)
    , ("Dec ",31) ] >>=
    (\(month, days) ->
      [1 .. days] >>=
        (\day -> return (month ++ show day) ) )
