module State where

data State = Wall | Empt | Ball
  deriving (Eq, Show, Read)

toChar :: State -> Char
toChar Wall = ' '
toChar Empt = '.'
toChar Ball = '#'

toString :: [State] -> String
toString states = map (toChar) states

toStrings :: [[State]] -> [String]
toStrings statess = map (toString) statess

toFormattedString :: [[State]] -> String
toFormattedString = unlines . toStrings