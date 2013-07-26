module SolutionTree where

import System.IO

import Board

{-
    > Big TODO:
    >  - Generate all move permutations
    >  - Work from the bottom up (with solutions that provide one marble left)
    >  - Create a set of solutions
    >  - Solve puzzle
-}

data Tree = Root { board :: Board, branches :: [Tree] } | Node { board :: Board, move :: Move, parent :: Tree, branches :: [Tree] }
  deriving (Eq, Show, Read)

generateTree :: Tree -> [Tree]
generateTree n
  | vms == [] = []
  | otherwise = [let nn = Node b m n $ generateTree nn in nn | (m, b) <- nbs]
  where b   = board n
        vms = stripSimilarMoves b $ validMoves b
        nbs = map (\x -> (x, makeMove b x)) vms

-- The root node
root :: Tree
root = Root trunkBoard $ generateTree root

-- Getting the move string that resulted in a board
getMoveStringRaw :: Tree -> [Move]
getMoveStringRaw t = undefined

getMoveString :: Tree -> [Move]
getMoveString t = reverse $ getMoveStringRaw t

-- Making a list of trees from a tree
makeList :: Tree -> [Tree]
makeList t@(Root board branch) = t : (concat $ map (makeList) branch)
makeList t@(Node board _ _ branch) = t : (concat $ map (makeList) branch)

-- Finding the solution to the game
findSolution :: [Tree] -> [Move]
findSolution [] = []
findSolution (x:xs)
  | getNumBalls b == 1 = getMoveString x
  | otherwise         = findSolution xs
  where b = board x