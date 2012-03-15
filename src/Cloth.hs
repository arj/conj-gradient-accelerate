{-#LANGUAGE TypeOperators #-}
module Cloth where

import Data.Array.Accelerate as Acc
import Data.Array.Accelerate.Math
import Data.Array.Accelerate.Math.SMVM
import Data.Array.Accelerate.Types
import Data.Array.Accelerate.Interpreter as I
import Debug.Trace (trace)
import Prelude hiding (replicate, zip, unzip, map, scanl, scanl1, scanr, scanr1, zipWith,
                         filter, max, min, not, fst, snd, curry, uncurry, sum, head, tail,
                         drop, take, null, length, reverse, init, last, product, minimum,
                         maximum)
import qualified Prelude

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

-- (Segments, Indices, Values)
type AccMultiSparseMatrix a = (AccSparseMatrix Int, (AccSparseMatrix Int, AccSparseMatrix a), AccVector Int)

type AccThreeTupleVector = Acc (Vector (Int,Int,Int))

-- | Extracts a row from a sparse matrix
--   n is 0-based
extractRow :: (Elt a) => Exp Int -> AccSparseMatrix a -> AccSparseVector a
extractRow n (segs, (idxs, vals), cols) = (takerow idxs, takerow vals, cols)
  where
    before     = the $ sum $ take n segs
    count      = segs Acc.! (index1 n)
    takerow xs = take count $ drop before xs

mpcgMultiInitialAcc :: AccMultiSparseMatrix Float -> AccSparseMatrix Float -> AccSparseMatrix Float -> Acc (Array DIM2 Float) -> Float -> Int -> Int -> AccThreeTupleVector
mpcgMultiInitialAcc a@(allsegs, (allidxs, allvals), allcols) allb allz allp epsilon n eqcount = Acc.map f eqcount'
  where
    eqcount' = use $ fromList (Z :. eqcount) [0..eqcount-1] :: AccVector Int
    --
    f :: Exp Int -> Exp (Plain (Int,Int,Int))
    f i = lift (i,i,i)
      where
        segs = extractRow i allsegs
        idxs = extractRow i allidxs
        vals = extractRow i allvals
        cols = allcols ! index1 i
        -- Extracting all the rows
        a    = (segs, (idxs, vals), cols)
        b    = extractRow i allb
        z    = extractRow i allz
        p    = slice allp (lift (Z :. i :. All))
        -- Real initialization
        p_inv   = vectorInverseAcc p
        dv      = z
        --delta0  = sparseDotPacc (filterMPCG b) (p *^ b)
        --r       = (Acc.zipWith (-) b (smvmAcc a dv))
        --c       = p_inv *^ r
        --delta   = dotpAcc r c

--        calc = mpcgMultiSingleStep
       
        
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

----------------------------------------
-- CURRENTLY NOT USED

-- | Fetches an entry from a sparse vector.
getEntry :: (Elt a) => Exp Int -> a -> AccSparseVector a -> Exp a
getEntry i d (idx,val,_) = Acc.snd $ the $ Acc.foldAll f def xs
  where
    xs  = Acc.zip idx val
    def = constant (0,d)
    f ack v = (i ==* Acc.fst v) ? (v, ack)

-- | Creates a vector from a sparse vector
vectorFromSparseVector :: (Elt a) => AccSparseVector a -> Int -> a -> AccVector a
vectorFromSparseVector sv@(idx,val,_) size d = Acc.map m def
  where
    def = use $ fromList (Z :. size) [1..size]
    --
    m i = getEntry i d sv 
