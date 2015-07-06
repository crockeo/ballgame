module Board where

import System.IO

import Control.Monad

import Data.Maybe
import Data.List

import Direction
import Position
import State
import Utils

data LineOfSymmetry = Vertical | Horizontal
  deriving (Eq, Show, Read)

type Board = ([State], Position)
type Move = (Position, Direction)

-- The trunk of the solution tree (how the game starts out)
trunkBoard :: Board
trunkBoard = ([Wall, Wall, Ball, Ball, Ball, Wall, Wall,
               Wall, Ball, Ball, Ball, Ball, Ball, Wall,
               Ball, Ball, Ball, Ball, Ball, Ball, Ball,
               Ball, Ball, Ball, Empt, Ball, Ball, Ball,
               Ball, Ball, Ball, Ball, Ball, Ball, Ball,
               Wall, Ball, Ball, Ball, Ball, Ball, Wall,
               Wall, Wall, Ball, Ball, Ball, Wall, Wall],
              (7, 7))


-- Converting between 2-d coords to 1-d coords (for an array)
_convert :: Board -> Position -> Int
_convert (_, (w, _)) (x, y) = (y * w) + x

-- Checking if a point is within the board
withinBoard :: Board -> Position -> Bool
withinBoard (_, (w, h)) (x, y) =
  (x >= 0 && x < w) && (y >= 0 && y < h)

-- Getting a piece from the board
getPiece :: Board -> Position -> State
getPiece board@(states, _) pos
  | not $ withinBoard board pos = Wall
  | otherwise                   = states !! (_convert board pos)

-- Setting a piece on the board (returning a new board with the modified piece)
setPiece :: Board -> Position -> State -> Board
setPiece board@(states, size) pos state
  | not $ withinBoard board pos = board
  | otherwise = ([if _convert board pos == n
                   then state
                   else states !! n | n <- [0 .. (length states) - 1]], size)

-- Getting the number of balls on a given board
getNumBalls :: Board -> Int
getNumBalls (states, _) = sum [1 | n <- states, n == Ball]

-- Rotating a given move
rotateMove :: Board -> Move -> Move
rotateMove (_, (bw, bh)) (pos, dir) =
  (translate (w, h) $ rotateClockwise $ translate pos (-w, -h), nextDirection dir)
  where w = bw `div` 2
        h = bh `div` 2

-- Flipping a move
flipOver :: Int -> Int -> Int
flipOver s n
  | n <  s = ((s * 2) - n)
  | n == s = n
  | n >  s = (s - (n - s))

-- Flipping a move over a line of symmetry
flipMove :: Board -> LineOfSymmetry -> Move -> Move
flipMove (_, (bw, bh)) line move@((x, y), dir)
  | (even bw) || (even bh) = move
  | line == Horizontal     = ((x, flipOver h y), oppositeDirection dir)
  | line == Vertical       = ((flipOver w x, y), oppositeDirection dir)
  where w = bw `div` 2
        h = bh `div` 2

-- Generating all rotated moves
generateRotatedMoves :: Board -> Move -> [Move]
generateRotatedMoves board m1 =
  [m1, m2, m3, m4]
  where rotate = rotateMove board
        m2     = rotate m1
        m3     = rotate m2
        m4     = rotate m3

-- Generating all flipped moves
generateFlippedMoves :: Board -> Move -> [Move]
generateFlippedMoves board m1 =
  [m1, m2, m3]
  where flip = flipMove board
        m2   = flip Vertical m1
        m3   = flip Horizontal m1

-- Checking if a move is valid
validMove :: Board -> Move -> Bool
validMove board move@(pos1@(x, y), dir)
  | not $ and $ map (\x -> withinBoard board x) [pos1, pos2, pos3] = False
  | not (p1 == Ball && p2 == Ball && p3 == Empt)                   = False
  | otherwise                                                      = True
  where pos2 = goDirN pos1 dir 1
        pos3 = goDirN pos1 dir 2
        p1   = getPiece board pos1
        p2   = getPiece board pos2
        p3   = getPiece board pos3

-- Generating a list of valid moves from a board
validMoves :: Board -> [Move]
validMoves board@(_, (w, h)) = [((x, y), dir) | x <- [0 .. w - 1],
                                                y <- [0 .. h - 1],
                                                dir <- [Upwards .. Rightwards],
                                                validMove board ((x, y), dir)]

-- Stripping rotated moves from a list of moves
stripSimilarMoves :: Board -> [Move] -> [Move]
stripSimilarMoves board moves =
  nub $ movess2
  where movess1 = map (head) $ nubBy (\x y -> elemAny x y) $ map (generateRotatedMoves board) moves
        movess2 = map (head) $ nubBy (\x y -> elemAny x y) $ map (generateFlippedMoves board) movess1

-- Getting a list of assymetric moves for a board
assymetricMoves :: Board -> [Move]
assymetricMoves board = stripSimilarMoves board $ validMoves board

-- Performing a move on a board
makeMove :: Board -> Move -> Board
makeMove board move@(pos1, dir)
  | not $ validMove board move = board
  | otherwise = setPiece (setPiece (setPiece board pos1 Empt) pos2 Empt) pos3 Ball
  where pos2 = goDirN pos1 dir 1
        pos3 = goDirN pos1 dir 2

-- Generating a string that could be putStrLn'd from a board
boardToOutputString :: Board -> String
boardToOutputString (states, (w, _)) = toFormattedString $ splitInto w states

-- Generating a string that could be putStrLn'd from a move
moveToOutputString :: Move -> String
moveToOutputString ((x, y), dir) = "(" ++ show x ++ ", " ++ show y ++ ") " ++ show dir