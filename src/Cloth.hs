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

type AccMultiMatrix a = (AccMatrix Int, (AccMatrix Int, AccMatrix a), AccVector Int)

type AccMatrix a = Acc (Array DIM2 a)


mpcgMultiSingleStep :: AccMatrix Float -> AccMultiMatrix Float -> AccMatrix Float -> AccMatrix Float -> AccMatrix Float -> AccVector Float -> AccVector Float -> Float -> Int -> AccMatrix Float
mpcgMultiSingleStep p_inv a dv r_in c delta delta0 epsilon n -- = dv
  | n == 0  = dv
--  | otherwise = cond ?| (mpcgMultiSingleStep p_inv a dv' r' c' delta' delta0 epsilon (pred n), dv)
  | otherwise = mpcgMultiSingleStep p_inv a dv' r' c' delta' delta0 epsilon (pred n)
  where
{-    epsilon_l = lift epsilon
    cond      = (>*) (the delta) (epsilon_l * epsilon_l * (the delta0))-}
    q         = smvmMulti a c  :: AccMatrix Float
    p_cq      = Acc.fold (+) 0 $ Acc.zipWith (*) c q :: AccVector Float
    alpha     = Acc.zipWith (/) delta p_cq :: AccVector Float
    alpha_c   = Acc.zipWith (*) (expand alpha) c  :: AccMatrix Float
    dv'       = Acc.zipWith (+) dv alpha_c :: AccMatrix Float
    alpha_q   = Acc.zipWith (*) (expand alpha) q :: AccMatrix Float
    r         = Acc.zipWith (-) r_in alpha_q :: AccMatrix Float
    s         = Acc.zipWith (*) p_inv r :: AccMatrix Float
    delta'    = Acc.fold (+) 0 $ Acc.zipWith (*) r s :: AccVector Float
    beta      = Acc.zipWith (/) delta' delta :: AccVector Float
    beta_c    = Acc.zipWith (*) (expand beta) c :: AccMatrix Float
    c'        = Acc.zipWith (+) s beta_c :: AccMatrix Float -- filter
    r' = r :: AccMatrix Float
    --
    expand a  = replicate (lift (Z:.All:.3 :: Z:.All:.Int)) a


mpcgMultiInitialAcc :: AccMultiMatrix Float -> AccMatrix Float -> AccMatrix Float -> AccMatrix Float -> Float -> Int -> Int -> AccMatrix Float
mpcgMultiInitialAcc a@(allsegs, (allidxs, allvals), allcols) b z p epsilon n eqcount = mpcgMultiSingleStep p_inv a dv r c delta delta0 epsilon n
  where
    p_inv  = Acc.map (1/) p
    dv     = z
    delta0 = Acc.fold (+) 0 $ Acc.zipWith (*) b $ Acc.zipWith (*) p b -- filter
    r      = Acc.zipWith (-) b $ smvmMulti a dv  -- filter
    c      = Acc.zipWith (*) p r -- filter
    delta  = Acc.fold (+) 0 $ Acc.zipWith (*) r c


-- TESTS --

testmpcgMulti2 n = mpcgMultiInitialAcc a b z p epsilon n eqcount
  where
    allsegs = use $ fromList (Z :. 2 :. 3) [3,3,3,3,3,3] :: AccMatrix Int
    allidxs = use $ fromList (Z :. 2 :. 9) [0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2] :: AccMatrix Int
    allvals = use $ fromList (Z :. 2 :. 9) [1.0,1.0,1.0,1.0,5.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,5.0,1.0,1.0,1.0,1.0] :: AccMatrix Float
    allcols = use $ fromList (Z :. 3) [3,3,3] :: AccVector Int
    --
    a = (allsegs,(allidxs,allvals),allcols)
    b = use $ fromList (Z :. 2 :. 3) [6,14,6,6,14,6] :: AccMatrix Float
    z = use $ fromList (Z :. 2 :. 3) [0,0,0,0,0,0] :: AccMatrix Float
    p = use $ fromList (Z :. 2 :. 3) [1,5,1,1,5,1] :: Acc (Array DIM2 Float)
    epsilon = 0.0000001
    eqcount = 2



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
----

getArgs = (a,b,z,p)
  where
    a = usesm $ fromArrayZero $ fromList (Z :. (3 :: Int) :. (3 :: Int)) ([1,1,1,1,5,1,1,1,1] :: [Float])
    b = use $ fromList (Z :. (3 :: Int)) ([6,14,6] :: [Float])
    z = use $ fromList (Z :. (3 :: Int)) ([0,0,0] :: [Float])
    p = use $ fromList (Z :. (3 :: Int)) ([1,5,1] :: [Float])

----------------------------------------
-- CURRENTLY NOT USED


-- | Returns the index in the array for a sparse vector entry.
getIndexEntry :: (Elt a) => Exp Int -> AccSparseVector a -> Exp Int
getIndexEntry i (idx,_,_) = fst $ scanr f def xs ! index1 0
  where
    def = constant (-1,0) :: Exp (Int, Int)
    idxes = generate (lift (Z :. size idx)) unindex1
    xs  = Acc.zip idxes idx
    --
    f :: Exp (Int, Int) -> Exp (Int, Int) -> Exp (Int, Int)
    f ack v = let (no, idx) = unlift v :: (Exp Int, Exp Int) in
              (i ==* idx) ? (v, ack)


-- | Fetches an entry from a sparse vector.
getEntry :: (Elt a) => Exp Int -> a -> AccSparseVector a -> AccScalar a
getEntry i d (idx,val,_) = let array = Acc.foldAll f def xs in
                           let (_,v) = Acc.unzip array in
                           v
  where
    xs  = Acc.zip idx val
    def = constant (0,d)
    f ack v = (i ==* Acc.fst v) ? (v, ack)



vectorFromSparseVector :: (Elt a) => Exp a -> AccSparseVector a -> AccVector a
vectorFromSparseVector d (idx,val,s) = permute const def f val
  where
    def = generate (index1 $ the s) (\_ -> d)
    --
    f ix = index1 (idx ! ix)




idx1 = use $ fromList (Z :. 3) [1,2,5] :: AccVector Int
val1 = use $ fromList (Z :. 3) [1,2,3] :: AccVector Float

sv1 = (idx1, val1, unit 6 :: AccScalar Int)

---------

allsegs = usesm $ fromArray 0 $ fromList (Z :. 2 :. 3) [3,3,3,3,3,3] :: AccSparseMatrix Int
allidxs = usesm $ fromArray 0 $ fromList (Z :. 2 :. 9) [0,1,2,0,1,2,0,1,2,0,1,2,0,1,2,0,1,2] :: AccSparseMatrix Int
allvals = usesm $ fromArrayZero $ fromList (Z :. 2 :. 9) [1.0,1.0,1.0,1.0,5.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,5.0,1.0,1.0,1.0,1.0] :: AccSparseMatrix Float
allcols = use $ fromList (Z :. 3) [3,3,3] :: AccVector Int
--
alla = (allsegs,(allidxs,allvals),allcols)
allb = usesm $ fromArrayZero $ fromList (Z :. 2 :. 3) [6,14,6,6,14,6] :: AccSparseMatrix Float
allz = usesm $ fromArrayZero $ fromList (Z :. 2 :. 3) [0,0,0,0,0,0] :: AccSparseMatrix Float
allp = use $ fromList (Z :. 2 :. 3) [1,5,1,1,5,1] :: Acc (Array DIM2 Float)


-- Unused indices, values, and segments have to be set to 0.
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

--
smvmTest = smvmMulti (segd,(idxs,vals),cnts) vecs
  where
    segd = use $ fromList (Z:.2:.3) [2,1,1,0,2,1] :: Acc (Array DIM2 Int)
    idxs = use $ fromList (Z:.2:.4) [0,2,2,0,1,2,0,0] :: Acc (Array DIM2 Int)
    vals = use $ fromList (Z:.2:.4) [1,5,3,2,1,1,1,0] :: Acc (Array DIM2 Float)
    cnts = use $ fromList (Z:.2) [3,3] :: Acc (Array DIM1 Int)
    vecs = use $ fromList (Z:.2:.3) [1,2,3,4,5,6] :: Acc (Array DIM2 Float)


