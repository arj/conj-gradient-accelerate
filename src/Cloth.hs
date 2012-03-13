{-#LANGUAGE TypeOperators #-}
module Cloth where

import Data.Array.Accelerate as Acc
import Data.Array.Accelerate.Math
import Data.Array.Accelerate.Math.SMVM
import Data.Array.Accelerate.Types
import Data.Array.Accelerate.Interpreter as I
import Debug.Trace (trace)

traceShow a = trace (show a) a
traceShowRun a = trace (show (I.run a)) a
traceShowRunStr s a = trace (s ++ "=" ++ (show (I.run a))) a

filterMPCG :: AccVector Float -> AccVector Float
filterMPCG v = v -- TODO Not implemented until now


mpcgSingleStep :: AccVector Float -> AccSparseMatrix Float -> AccVector Float -> AccVector Float -> AccVector Float -> AccScalar Float -> AccScalar Float -> Float -> Int ->
  AccVector Float
mpcgSingleStep p_inv a dv r_in c delta delta0 epsilon n
  | n == 0  = dv
  | otherwise = cond ?| (mpcgSingleStep p_inv a dv' r' c' delta' delta0 epsilon (pred n), dv)
  where
    epsilon_l = lift epsilon
    cond      = (>*) (the delta) (epsilon_l * epsilon_l * (the delta0))
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
mpcgInitial a' b' z' p' epsilon n = mpcgSingleStep p_inv a dv r c delta delta0 epsilon n
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


mpcgInitialAcc :: AccSparseMatrix Float -> AccVector Float -> AccVector Float -> AccVector Float -> Float -> Int -> AccVector Float
mpcgInitialAcc a b z p epsilon n = mpcgSingleStep p_inv a dv r c delta delta0 epsilon n
  where
    p_inv   = vectorInverseAcc p
    dv      = z
    delta0  = dotpAcc (filterMPCG b) (p *^ b)
    r       = filterMPCG (Acc.zipWith (-) b (smvmAcc a dv))
    c       = p_inv *^ r
    delta   = dotpAcc r c

-----------------------------------------------------------

-- TODO These function should probably be moved to a library

-- *********** Index manipulations

-- | Applys a function op with first operand beeing the shape in
-- the first dimension and the second operand beeing n and repacks
-- it in a shape.
index1op :: Exp (Z :. Int) -> (Exp Int -> t -> Exp Int) -> t -> Exp (Z :. Int)
index1op ix op n = index1 (op (unindex1 ix) n)


-- *********** Actions on Acc Vectors

accSum :: (Elt a, IsNum a) => AccVector a -> AccScalar a
accSum xs = Acc.foldAll (+) 0 xs

accTake :: (Elt a) => Exp Int -> AccVector a -> AccVector a
accTake n xs = Acc.backpermute (index1 n) (\x -> x) xs

accDrop :: (Elt a) => Exp Int -> AccVector a -> AccVector a
accDrop n xs = Acc.backpermute (index1op (shape xs) (-) n) (\i -> index1op i (+) n) xs





-- (Segments, Indices, Values)
type AccMultiSparseMatrix a = (AccSparseMatrix Int, (AccSparseMatrix Int, AccSparseMatrix a))

type AccThreeTupleVector = Acc (Vector (Int,Int,Int))

-- | Extracts a row from a sparse matrix
-- | n is 1-based, not 0-based!
extractRow :: (Elt a) => Exp Int -> AccSparseMatrix a -> AccSparseVector a
extractRow n (segs, (idxs, vals)) = (takerow idxs, takerow vals)
  where
    before     = traceShow (the $ accSum $ accTake (n-1) segs)
    count      = segs Acc.! (index1 (n + 1))
    takerow xs = accTake count $ accDrop before xs

-- mpcgMultiInitialAcc :: AccMultiSparseMatrix Float -> AccMultiVector Float -> AccMultiVector Float -> AccMultiVector Float -> Float -> Int -> Int -> AccThreeTupleVector
-- mpcgMultiInitialAcc a b z p epsilon n eqcount = Acc.map f eqcount'
mpcgMultiInitialAcc epsilon n eqcount = Acc.map f eqcount'
  where
    eqcount' = use $ fromList (Z :. eqcount) [1..eqcount] :: AccVector Int
    --
    f :: Exp Int -> Exp (Plain (Int,Int,Int))
    f i = lift (n,n,n)

-----------------------------------------------------------

test n = mpcgInitial a b z p e n
  where
    a = fromArrayZero $ fromList (Z :. (3 :: Int) :. (3 :: Int)) ([1,1,1,1,5,1,1,1,1] :: [Float])
    b = fromList (Z :. (3 :: Int)) ([6,14,6] :: [Float])
    z = fromList (Z :. (3 :: Int)) ([0,0,0] :: [Float]) 
    p = fromList (Z :. (3 :: Int)) ([1,5,1] :: [Float])
    e = 0.0000000001


testAcc n = mpcgInitialAcc a b z p e n
  where
    a = usesm $ fromArrayZero $ fromList (Z :. (3 :: Int) :. (3 :: Int)) ([1,1,1,1,5,1,1,1,1] :: [Float])
    b = use $ fromList (Z :. (3 :: Int)) ([6,14,6] :: [Float])
    z = use $ fromList (Z :. (3 :: Int)) ([0,0,0] :: [Float]) 
    p = use $ fromList (Z :. (3 :: Int)) ([1,5,1] :: [Float])
    e = 0.0000000001



getArgs = (a,b,z,p)
  where
    a = usesm $ fromArrayZero $ fromList (Z :. (3 :: Int) :. (3 :: Int)) ([1,1,1,1,5,1,1,1,1] :: [Float])
    b = use $ fromList (Z :. (3 :: Int)) ([6,14,6] :: [Float])
    z = use $ fromList (Z :. (3 :: Int)) ([0,0,0] :: [Float]) 
    p = use $ fromList (Z :. (3 :: Int)) ([1,5,1] :: [Float])
