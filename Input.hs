module Input where

import System.IO
import Data.Maybe

import Direction
import Utils

_getInput :: Char -> Int -> IO Int
_getInput coord cap = do
  putStr outString
  hFlush stdout
  
  n <- getLine
  if allDigit n
    then let rn = ((read n) :: Int) in
      if (rn >= 0) && (rn < cap)
        then return rn
        else _getInput coord cap
    else _getInput coord cap
  where outString = "Enter " ++ [coord] ++ "-coordinate (" ++ [coord] ++ " >= 0 & " ++ [coord] ++ " < " ++ show cap ++ ") : "

_toDirection :: String -> IO (Maybe Direction)
_toDirection s
  | tls == "up"    = return $ Just Upwards
  | tls == "down"  = return $ Just Downwards
  | tls == "left"  = return $ Just Leftwards
  | tls == "right" = return $ Just Rightwards
  | otherwise    = return   Nothing
  where tls = allLower s

getX :: Int -> IO Int
getX n = _getInput 'x' n

getY :: Int -> IO Int
getY n = _getInput 'y' n

getDirection :: IO Direction
getDirection = do
  putStr "Enter direction (up, down, left, right): "
  hFlush stdout

  dirs <- getLine
  dir <- _toDirection dirs

  if dir == Nothing
    then getDirection
    else return $ fromJust dir