module Looptest where

import Data.Array.Accelerate           (Vector, Segments, Acc, use)
import qualified Data.Array.Accelerate as Acc
import Data.Array.Accelerate.Math
import Data.Array.Accelerate.Math.SMVM
import Data.Array.Accelerate.Types


loopTest :: AccVector Int -> AccScalar Int
loopTest v = Acc.fold (+) 1 v

loopTest2 :: AccScalar Int -> AccScalar Int
loopTest2 n = Acc.cond (1 Acc.<* (Acc.the n)) (loopTest2 (Acc.map (-1) n)) 0
