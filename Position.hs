module Position where

import Direction

type Position = (Int, Int)
type RotationMatrix = (Int, Int, Int, Int)

_90matrix :: RotationMatrix
_90matrix = (0, -1, 1, 0)

_mulMatrix :: Position -> RotationMatrix -> Position
_mulMatrix (x, y) (a, b, c, d) = ((x * a) + (y * b), (x * c) + (y * d))

rotateClockwise :: Position -> Position
rotateClockwise pos = _mulMatrix pos _90matrix

toPosition :: Direction -> Position
toPosition Upwards    = ( 0, -1)
toPosition Downwards  = ( 0,  1)
toPosition Leftwards  = (-1,  0)
toPosition Rightwards = ( 1,  0)

translate :: Position -> Position -> Position
translate (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

scalar :: Position -> Int -> Position
scalar (x1, y1) n = (x1 * n, y1 * n)

goDirN :: Position -> Direction -> Int -> Position
goDirN pos dir n =
  translate pos pdir
  where pdir = scalar (toPosition dir) n