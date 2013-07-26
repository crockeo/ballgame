module Game where

import System.IO

import Board
import Input

_makeMove :: Board -> IO Move
_makeMove (_, (w, h)) = do
  x <- getX w
  y <- getY h
  dir <- getDirection

  return ((x, y), dir)

start :: IO Int
start = loop trunkBoard

end :: IO Int
end = return 0

loop :: Board -> IO Int
loop board = do
  if getNumBalls board == 0
    then do
      putStrLn "You win!!!"
      hFlush stdout

      end
    else do
      putStrLn $ toOutputString board
      hFlush stdout

      move <- _makeMove board
      if validMove board move
        then loop $ makeMove board move
        else loop board