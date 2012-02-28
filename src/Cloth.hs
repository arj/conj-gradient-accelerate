module Cloth where

import Data.Array.Accelerate  --         (Vector, Segments, Acc, use, (?))
import qualified Data.Array.Accelerate as Acc
import Data.Array.Accelerate.Math
import Data.Array.Accelerate.Math.SMVM
import Data.Array.Accelerate.Types


-- TODO Maybe this should return Acc (Vector Float)?
filterMPCG :: AccVector Float -> AccVector Float
filterMPCG v = v -- TODO Not implemented until now


mpcgSingleStep :: AccSparseMatrix Float -> AccVector Float -> AccVector Float -> AccVector Float -> AccScalar Float ->
  (AccVector Float, AccVector Float, AccVector Float, AccScalar Float)
mpcgSingleStep a dv r c delta = (dv', r', c', delta')
  where
    p_inv     = a -- TODO
    q         = filterMPCG (smvmAcc a c)
    p_cq      = dotpAcc c q
--    alpha   = rho / -- TODO
    alpha     = use $ Acc.fromList (Z) [1]   :: AccScalar Float
    alpha_c   = alpha *. c                   :: AccVector Float
    dv'       = Acc.zipWith (+) dv alpha_c   :: AccVector Float
    alpha_q   = alpha *. q
    r         = Acc.zipWith (-) r alpha_q
    s         = smvmAcc p_inv r 
    delta'    = dotpAcc r s                  :: AccScalar Float
    beta      = Acc.zipWith (/) delta' delta :: AccScalar Float
    beta_c    = beta *. c                    :: AccVector Float
    c'        = filterMPCG (Acc.zipWith (+) s beta_c)
    r' = r


-- Compute 'dv' in 'A * dv = B' with preconditioned conjugate gradient method.
--
-- * SparseMatrix in compressed row format
--
-- * b' given as 'normal' vector
--
-- * p is ...
--
-- * z is ...
--
-- * epsilon is the maximal error
--
-- * z is ...
--
-- mpcgAcc :: AccSparseMatrix Float -> AccVector Float -> AccSparseMatrix Float -> AccVector Float
--   -> AccScalar Float -> Int -> AccVector Float
-- mpcgAcc a b p z epsilon count = step dv r c rho_new count
--   where
--     p_inv   = p -- vectorInverseAcc p -- TODO This is a matrix, not a vector!
--     dv      = z
--     rho     = dotpAcc (filterMPCG b) (smvmAcc p b)
--     r       = filterMPCG (Acc.zipWith (-) b (smvmAcc a dv))
--     c       = smvmAcc p_inv r
--     rho_new = dotpAcc r c
-- 
--     stop    = (Acc.>* (Acc.the epsilon) ^ 2 * (Acc.the rho_new)) -- Partial function!
-- 
--     step :: AccVector Float -> AccVector Float -> AccVector Float -> AccScalar Float -> Int -> AccVector Float
--     step dv r c rho n =
--       (?)  (stop rho Acc.||* n == 0) (dv,(step dv' r' c' rho' (n - 1)))
--       where
--         dv'  = dv
--         r'   = r
--         c'   = c
--         rho' = rho_new

        
--
--
--mpcg2Acc :: SparseMatrix Float -> Vector Float -> SparseMatrix Float -> Vector Float -> Float -> Int -> Acc (Vector Float)
--mpcg2Acc a'@(a_segd', (a_inds', a_vals')) b' p'@(p_segd', (p_inds', p_vals')) z' epsilon' count' =
--    let 
--        a_segd  = use a_segd'
--        a_inds  = use a_inds'
--        a_vals  = use a_vals'
--        a       = (a_segd, (a_inds, a_vals))
--        b       = use b'
--        p_segd  = use p_segd'
--        p_inds  = use p_inds'
--        p_vals  = use p_vals'
--        p       = (p_segd, (p_inds, p_vals))
--        z       = use z'
--        epsilon = use epsilon'
--        count   = use count'
--
--     in
--        mpcgAcc a b p z epsilon count
