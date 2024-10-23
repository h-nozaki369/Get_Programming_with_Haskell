listMain :: [String]
listMain = do
    size1 <- [18.0,16.0] 
    cost1 <- [20.0,15.0]
    size2 <- [19.0,17.0] 
    cost2 <- [21.0,16.0]
    let pizza1 = (size1,cost1)
    let pizza2 = (size2,cost2)
    let betterPizza = comparePizzas pizza1 pizza2
    return (describePizza betterPizza)
