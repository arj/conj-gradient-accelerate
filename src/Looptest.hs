module Looptest where

import Data.Array.Accelerate           (Vector, Segments, Acc, use)
import qualified Data.Array.Accelerate as Acc
import Data.Array.Accelerate.Math
import Data.Array.Accelerate.Math.SMVM
import Data.Array.Accelerate.Types


loopTest :: AccVector Int -> AccScalar Int
loopTest v = Acc.fold (+) 1 v

loopTest2 :: Int -> AccVector Int -> AccScalar Int
loopTest2 n vs
 | n == 0    = Acc.fold (+) 1 vs
 | otherwise = loopTest2 (n-1) vs 
