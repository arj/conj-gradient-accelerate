module Cloth where

import Data.Array.Accelerate as Acc
import Data.Array.Accelerate.Math
import Data.Array.Accelerate.Math.SMVM
import Data.Array.Accelerate.Types


filterMPCG :: AccVector Float -> AccVector Float
filterMPCG v = v -- TODO Not implemented until now


mpcgSingleStep :: AccVector Float -> AccSparseMatrix Float -> AccVector Float -> AccVector Float -> AccVector Float -> AccScalar Float -> Int ->
  AccVector Float
mpcgSingleStep p_inv a dv r_in c delta n
  | n == 0    = dv
  | otherwise = mpcgSingleStep p_inv a dv' r' c' delta' (pred n)
  where
    q         = filterMPCG (smvmAcc a c)
    p_cq      = dotpAcc c q
    alpha     = delta /. p_cq
    alpha_c   = alpha *. c 
    dv'       = Acc.zipWith (+) dv alpha_c
    alpha_q   = alpha *. q
    r         = Acc.zipWith (-) r_in alpha_q
    s         = p_inv *^ r
    delta'    = dotpAcc r s
    beta      = Acc.zipWith (/) delta' delta
    beta_c    = beta *. c
    c'        = filterMPCG (Acc.zipWith (+) s beta_c)
    r' = r


mpcgInitial :: SparseMatrix Float -> Vector Float -> Vector Float -> Vector Float -> Float -> Int -> AccVector Float
mpcgInitial a' b' z' p' epsilon n = mpcgSingleStep p_inv a dv r c delta n
  where
    a       = usesm a'
    b       = use b'
    p       = use p'
    p_inv   = vectorInverseAcc p
    dv      = use $ z'
    delta0  = dotpAcc (filterMPCG b) (p *^ b)
    r       = filterMPCG (Acc.zipWith (-) b (smvmAcc a dv))
    c       = p_inv *^ r
    delta   = dotpAcc r c


-----------------------------------------------------------

bug = mpcgInitial a b z p e n
  where
    a = fromArrayZero $ fromList (Z :. (3 :: Int) :. (3 :: Int)) ([1,1,1,1,5,1,1,1,1] :: [Float])
    b = fromList (Z :. (3 :: Int)) ([6,14,18] :: [Float])
    z = fromList (Z :. (3 :: Int)) ([0,0,0] :: [Float]) 
    p = fromList (Z :. (3 :: Int)) ([1,5,1] :: [Float])
    e = 0
    n = 10
