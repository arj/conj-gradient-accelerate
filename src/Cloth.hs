{-#LANGUAGE TypeOperators #-}
module Cloth where

import Data.Array.Accelerate as Acc hiding (flatten)
import Data.Array.Accelerate.Math
import Data.Array.Accelerate.Math.SMVM
import Data.Array.Accelerate.Types
import Data.Array.Accelerate.Interpreter as I
import Debug.Trace (trace)
import Prelude hiding (replicate, zip, unzip, map, scanl, scanl1, scanr, scanr1, zipWith,
                         filter, max, min, not, fst, snd, curry, uncurry, sum, head, tail,
                         drop, take, null, length, reverse, init, last, product, minimum,
                         maximum,zip3)
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

type AccMultiMatrix a = (AccMatrix Int, (AccMatrix Int, AccMatrix a), AccVector Int)

type AccMatrix a = Acc (Array DIM2 a)


mpcgMultiSingleStep :: AccMatrix Float -> AccMultiMatrix Float -> AccMatrix Float -> AccMatrix Float -> AccMatrix Float -> AccVector Float -> AccVector Float -> AccVector Float -> Int -> AccMatrix Float
mpcgMultiSingleStep p_inv a dv r_in c delta delta0 e_sq n
  | n == 0  = dv
  | otherwise = mpcgMultiSingleStep p_inv a dv_cond r c' delta' delta0 e_sq (pred n)
  where
    cond      = Acc.zipWith (-) delta $ Acc.zipWith (*) delta0 e_sq -- > 0 another round, <= 0 otherwise 
    q         = smvmMulti a c
    p_cq      = Acc.fold (+) 0 $ Acc.zipWith (*) c q
    alpha     = Acc.zipWith (/) delta p_cq
    alpha_c   = Acc.zipWith (*) (expand alpha) c
    dv'       = Acc.zipWith (+) dv alpha_c
    alpha_q   = Acc.zipWith (*) (expand alpha) q
    r         = Acc.zipWith (-) r_in alpha_q
    s         = Acc.zipWith (*) p_inv r
    delta'    = Acc.fold (+) 0 $ Acc.zipWith (*) r s
    beta      = Acc.zipWith (/) delta' delta
    beta_c    = Acc.zipWith (*) (expand beta) c
    c'        = Acc.zipWith (+) s beta_c -- filter
    conddvdv' = zip3 (expand cond) dv dv'
    dv_cond   = Acc.map condfun conddvdv'
    --
    expand a  = replicate (lift (Z:.All:.3 :: Z:.All:.Int)) a
    --
    condfun a = let (cond,dv,dv') = unlift a :: (Exp Float, Exp Float, Exp Float) in
                (cond >* 0) ? (dv',dv)


mpcgMultiInitialAcc :: AccMultiMatrix Float -> AccMatrix Float -> AccMatrix Float -> AccMatrix Float -> Float -> Int -> AccMatrix Float
mpcgMultiInitialAcc a@(allsegs, (allidxs, allvals), allcols) b z p epsilon n = mpcgMultiSingleStep p_inv a dv r c delta delta0 e_sq n
  where
    p_inv  = Acc.map (1/) p
    dv     = z
    delta0 = Acc.fold (+) 0 $ Acc.zipWith (*) b $ Acc.zipWith (*) p b -- filter
    r      = Acc.zipWith (-) b $ smvmMulti a dv  -- filter
    c      = Acc.zipWith (*) p_inv r -- filter
    delta  = Acc.fold (+) 0 $ Acc.zipWith (*) r c
    e_sq   = replicate (shape delta0) $ unit $ constant $ epsilon * epsilon


-- * Helper functions. Should be put in Acc Prelude.

-- Unused indices, values, and segments have to be set to 0.
-- All segments must be given!
smvmMulti :: AccMultiMatrix Float -> AccMatrix Float -> AccMatrix Float
smvmMulti (segs, (idxs, vals), cnt) vecs = reshape (shape vecs) $ foldSeg (+) 0 (flatten products) (flatten segs)
  where
    vecVals         = backpermute (shape idxs) (\ix -> index2 (fst $ unindex2 ix) $ idxs Acc.! ix) vecs
    products        = Acc.zipWith (*) vecVals vals


index2 :: Exp Int -> Exp Int -> Exp DIM2
index2 i j      = lift (Z :. i :. j)

unindex2 :: Exp DIM2 -> Exp (Int, Int)
unindex2 ix     = let Z :. i :. j = unlift (ix :: Exp DIM2) in lift ((i,j) :: (Exp Int, Exp Int))

-- | Flattens a given array of arbitrary dimension.
--
flatten :: (Shape ix, Elt a) => Acc (Array ix a) -> Acc (Array DIM1 a)
flatten a = reshape (index1 $ size a) a


-- | Takes three arrays and produces an array of a three-tuple.
--   TODO Maybe there is a better way to implement this but with 2 zips?!
--
zip3 :: forall a b c sh. (Shape sh, Elt c, Elt b, Elt a) =>
     Acc (Array sh a) -> Acc (Array sh b) -> Acc (Array sh c) -> Acc (Array sh (a, b, c))
zip3 a b c = Acc.zipWith f a $ Acc.zip b c
  where
    f a bc = let (b,c) = unlift bc :: (Exp b, Exp c) in
             lift (a, b, c) :: Exp (a,b,c)
