{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
module TwentyFourtyEight where

import System.Random

--------------------------------------------------------------------------------
-- Grid
--------------------------------------------------------------------------------

type Row = [Int]
type Grid = [Row]


showGrid :: Grid -> String
showGrid [] = ""
showGrid (row : rows) = (concatMap ((++ " ") . show) row) ++ "\n" ++ showGrid rows

initial :: Grid
initial = replicate 4 [0, 0, 0, 0]

-- >>> print . showGrid $ initial
-- "0 0 0 0 \n0 0 0 0 \n0 0 0 0 \n0 0 0 0 \n"

--------------------------------------------------------------------------------
-- Grid rotation
--------------------------------------------------------------------------------

data Degree = Zero | Ninety | OneEighty | TwoSeventy deriving Show

rotate :: Degree -> Grid -> Grid
rotate Zero    grid = grid
rotate Ninety  (row : rows) = _

{-------------------------------------------------------------------------------
Grid actions
------------

We need only implement the Left action; all other inputs are the the composition
of rotating the grid, performing the left action, then unrotating.

-------------------------------------------------------------------------------}



unfill, refill, combine :: Row -> Row
unfill = filter (/= 0)

-- >>> unfill [4, 0, 2, 0]
-- [4,2]

refill xs
  | length xs == 4 = xs
  | otherwise = xs ++ replicate (4 - length xs) 0

-- >>> refill [4, 2]
-- [4,2,0,0]

combine [] = []
combine [x] = [x]
combine (x : y: ys)
  | x == y    = (x + y) : combine ys
  | otherwise = x : combine (y : ys)

-- >>> combine [2, 2, 2, 2]
-- [4,4]

left :: Grid -> Grid
left = map (refill . combine . unfill)


-- >>> left [[2, 8, 2, 2]]
-- [[2,8,4,0]]

data Action = U | D | L | R

-- work :: Action -> Grid -> Grid
-- work _ (Grid [])           = Grid []
-- work Left (Grid rows)      = map lCombine rows
-- work Up (Grid (row : rows)) = _


--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

-- populateEmptySpace :: Grid -> IO Grid
-- populateEmptySpace grid = do
--   seed <- newStdGen
--   r <- randomRIO (False, True)
--   case row of
    
--   return r

  
-- getEmptySpaces (row : rows) = 

-- getRandomSpace :: Grid -> IO (Int, Int)
-- getRandomSpace = do
--   g <- newStdGen
--   let r = (0, 3) :: (Int, Int)
--   return . fst $ (randomR r g)

main :: IO ()
main = do
 return ()

-- >>> :t random
-- random :: (Random a, RandomGen g) => g -> (a, g)
