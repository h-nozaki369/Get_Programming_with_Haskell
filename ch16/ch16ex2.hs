data Circle = Circle {
    radius :: Double
}

data Square = Square {
    edge :: Double
}

data Rectangle = Rectangle {
    shortEdge :: Double
  , longEdge  :: Double
}

data Shape = CircleShape Circle
           | SquareShape Square
           | RectangleShape Rectangle

perimeter :: Shape -> Double
perimeter (CircleShape circle) = (radius circle) * 2 * 3.14
perimeter (SquareShape square) = (edge square) * 4
perimeter (RectangleShape rectangle) = ((shortEdge rectangle) + (longEdge rectangle)) * 2

area :: Shape -> Double
area (CircleShape circle) = (radius circle)^2 * 3.14
area (SquareShape square) = (edge square)^2
area (RectangleShape rectangle) = (shortEdge rectangle) * (longEdge rectangle)
