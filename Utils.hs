module Utils where

import Data.Char

elemAny :: (Eq a) => [a] -> [a] -> Bool
elemAny l1 l2 = or $ map (\x -> elem x l2) l1

splitInto :: Int -> [a] -> [[a]]
splitInto n list
  | length list <= n = [list]
  | otherwise        = fst split : splitInto n (snd split)
  where split = splitAt n list

allDigit :: String -> Bool
allDigit s = and $ map (isDigit) s

allLower :: String -> String
allLower s = map (toLower) s