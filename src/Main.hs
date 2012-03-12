{-# LANGUAGE CPP, BangPatterns #-}

module Main where

import Cloth
import Looptest
import Config
import System.Environment
import Data.Array.Accelerate      ( Array, Scalar, Exp, Acc,DIM1,DIM0,DIM2, Z(..), (:.)(..) )
import qualified Data.Array.Accelerate as A
#ifdef ACCELERATE_CUDA_BACKEND
import qualified Data.Array.Accelerate.CUDA as CUDA
#endif

-- Main
-- ====

-- main :: IO ()
-- main = do
--    putStrLn "Just Nothing"

main :: IO ()
main = do

  (config, nops) <- processArgs =<< getArgs

--  !_ <- return $ run config loopTest (A.fromList (Z :. (5 :: Int)) [0,1,2,3,4])
  putStrLn $ show $ run config loopTest2Ten (A.fromList (Z :. (5 :: Int)) [0,1,2,3,4])
  where
    loopTest2Ten = loopTest2 100
