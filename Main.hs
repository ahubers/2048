module Main where

import System.Random
import System.IO

import Control.Monad.Trans
import Control.Monad.State

import TwentyFourtyEight

--------------------------------------------------------------------------------
-- main
--
-- Usage:
-- run main. {w, a, s, d} + enter for up, left, down, right, resp.
--------------------------------------------------------------------------------


getKeyAction :: IO (Action)
getKeyAction = do
  c <- getLine
  case c of
    "w"  -> return U
    "a"  -> return L
    "s"  -> return D
    "d"  -> return R
    _    -> return Exit
  
run :: StateT Grid IO ()
run = do
  grid <- get
  liftIO $ putStrLn (showGrid grid)
  action <- liftIO getKeyAction
  let grid' = work action grid
  if grid == grid' then run else do
    grid' <- liftIO $ insertRandom grid'
    put grid'
    run

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  runStateT run initial >> return ()
    
