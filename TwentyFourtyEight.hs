module TwentyFourtyEight where

import System.Random
import Data.List
import Data.Char (digitToInt)

--------------------------------------------------------------------------------
-- Grid
--------------------------------------------------------------------------------

type Row = [Int]
type Grid = [Row]


showGrid :: Grid -> String
showGrid [] = ""
showGrid (row : rows) =
  (concatMap ((++ " ") . show) row) ++ "\n" ++ showGrid rows

initial :: Grid
initial = map (map digitToInt) [
  "0000",
  "0200",
  "0000",
  "0200"
  ]
  
-- >>> putStrLn . showGrid $ initial
-- 0 0 0 0 
-- 0 0 0 0 
-- 0 0 0 0 
-- 0 0 0 2

--------------------------------------------------------------------------------
-- Grid rotation
--
-- `rotate n` rotates the grid counter-clockwise n times.
--------------------------------------------------------------------------------

rotate :: Int -> Grid -> Grid
rotate 0 grid = grid
rotate 1 grid = reverse . transpose $ grid
rotate n grid
 | n <= 3    = rotate (n - 1) (rotate 1 grid) 
 | otherwise = rotate (n `mod` 4) grid
 
{-------------------------------------------------------------------------------
Grid actions
------------

We need only implement the Left action; all other inputs are the the composition
of rotating the grid, performing the left action, then unrotating.

-------------------------------------------------------------------------------}

flatten :: Grid -> Row
flatten = concat

inflate :: Row -> Grid
inflate [] = []
inflate xs = (take 4 xs) : (inflate (drop 4 xs))

unfill, refill, combine :: Row -> Row
unfill = filter (/= 0)

refill xs
  | length xs == 4 = xs
  | otherwise = xs ++ replicate (4 - length xs) 0

combine [] = []
combine [x] = [x]
combine (x : y: ys)
  | x == y    = (x + y) : combine ys
  | otherwise = x : combine (y : ys)

left :: Grid -> Grid
left = map (refill . combine . unfill)


data Action = L | R | U | D | Exit deriving (Show, Eq)

work :: Action -> Grid -> Grid
work L = left 
work U = (rotate 3) . left . (rotate 1)
work D = (rotate 1) . left . (rotate 3)
work R = (rotate 2) . left . (rotate 2)

--------------------------------------------------------------------------------
-- Inefficient grid insertion shenanigans
--------------------------------------------------------------------------------

insert2 :: Int -> Grid -> Grid
insert2 i xs = inflate $ take i flat ++ [2] ++ drop (i+1) flat
  where
    flat = flatten xs

emptyAtPos :: Int -> Grid -> Bool
emptyAtPos n grid = ((flatten grid) !! n) == 0

insertRandom :: Grid -> IO (Grid)
insertRandom grid = do
  g <- newStdGen
  let r = (0, 15) :: (Int, Int)
  let i = fst $ randomR r g
  if emptyAtPos i grid
    then return (insert2 i grid)
    else insertRandom grid
