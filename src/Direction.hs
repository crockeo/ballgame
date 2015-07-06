module Direction where

data Direction = Upwards | Downwards | Leftwards | Rightwards
  deriving (Eq, Enum, Show, Read)

nextDirection :: Direction -> Direction
nextDirection Upwards    = Rightwards
nextDirection Downwards  = Leftwards
nextDirection Leftwards  = Upwards
nextDirection Rightwards = Downwards

oppositeDirection :: Direction -> Direction
oppositeDirection Upwards    = Downwards
oppositeDirection Downwards  = Upwards
oppositeDirection Leftwards  = Rightwards
oppositeDirection Rightwards = Leftwards