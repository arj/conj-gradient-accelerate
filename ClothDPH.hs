{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS -fvectorise #-}

module Cloth (mpcgPA) where

import Vec3
import Vector

import Data.Array.Parallel
import Data.Array.Parallel.Prelude.Double hiding ((+), (-), (*), (/), (>), sumP)
import qualified
       Data.Array.Parallel.Prelude.Double as D
import Data.Array.Parallel.Prelude.Int    hiding ((+), (-), (*), (>), sumP)
import qualified
       Data.Array.Parallel.Prelude.Int    as I

import qualified Prelude as P



-- Wrapper
-- -------

-- Wrapper to use 'mpcg' from non-vectorised code.
--
mpcgPA :: SparseMatrixPA -> VectorPA -> VectorPA -> VectorPA -> Double -> Int -> VectorPA
{-# NOINLINE mpcgPA #-}
mpcgPA a b p z epsilon count 
  = toPArrayP
      (mpcg (fromNestedPArrayP a) (fromPArrayP b) (fromPArrayP p) (fromPArrayP z) epsilon count)

-- Compute 'dv' in 'a * dv = b' using Baraff & Witkin's modified preconditioned conjugate gradient
-- method.
--
-- * The sparse, symmetric, and positive-definite matrix 'a' describes the physical properties of
--   the cloth.
--
-- * The value 'epsilon' determines the tolerance on the solution error. (Moreover, 'count' gives
--   a strict upper bound on the number of iterations.)
--
-- * The vector 'p' is the diagonal of a preconditioner matrix P, where P_ii = 1/A_ii, where the
--   matrix A corresponds to 'a'.
--
mpcg :: SparseMatrix -> Vector -> Vector -> Vector -> Double -> Int -> Vector
mpcg a b p z epsilon count = cgStep dv0 r0 c0 rho1 count
  where
    pinv = mapP (vec3 1 /) p

    dv0  = z
    rho0 = dotp (constrain b) (p *^ (constrain b))
    r0   = constrain (b -^ smvm a dv0)
    c0   = constrain (pinv *^ r0)
    rho1 = dotp r0 c0
    
    stop = (> vec3 (epsilon D.* epsilon) * rho0)
    constrain v = v :: Vector  -- !!!

    cgStep dv r c rho n
      | stop rho || n I.== 0 = dv
      | otherwise            = cgStep dv' r' c' rho' (n I.- 1)
      where
        q     = constrain (smvm a c)
        alpha = rho / dotp c q

        dv'   = dv +^ mapP (alpha *) c
        r'    = r -^ mapP (alpha *) q
        s'    = pinv *^ r'
        rho'  = dotp r' s'

        beta  = rho' / rho
        c'    = constrain (s' +^ mapP (beta *) c)
