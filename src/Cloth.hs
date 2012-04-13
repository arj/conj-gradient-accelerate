{-#LANGUAGE TypeOperators,ScopedTypeVariables #-}
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
import qualified Triangular

traceShow a = trace (show a) a
traceShowRun a = trace (show (I.run a)) a
traceShowRunStr s a = trace (s ++ "=" ++ (show (I.run a))) a

-- | Implements the filter function as described in 5.3
--
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

expand :: Elt e => Acc (Array (Z :. Int) e) -> Acc (Array ((Z :. Int) :. Int) e)
expand a  = replicate (lift (Z:.All:.3 :: Z:.All:.Int)) a

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


-------------------------------------------------------

-- Maybe use sparse matrices for particle state?
generateParticles :: Int -> Int -> Acc (Array DIM2 Float)--, AccMatrix Int, AccMatrix Int)
generateParticles m n = state
  where
    state = generate (lift (Z :. m*n :. 3 :: Z :. Int :. Int)) statefun
    --
    statefun ix = let ip = unindex2 ix in
                let i = fst ip in
                let p = snd ip in
                let x = Acc.fromIntegral $ i `mod` constant m in
                let y = Acc.fromIntegral $ i `div` constant m in
                (p ==* 0) ? (x, (p ==* 1) ? (y, 0))
                
xyz [] = []                
xyz ((x,y,z,dx,dy,dz) : xs) = (x,dx) : (y,dy) : (z,dz) : xyz xs

-- Eq. 9, right part
getInverseUVMatrix uv triangles = Acc.map f triangles
  where
    f :: Exp (Int, Int, Int) -> Exp (Float, Float, Float, Float)
    f ijk = let (i,j,k) = unlift ijk :: (Exp Int, Exp Int, Exp Int) in
            let (u_i,v_i) = unlift (uv ! index1 i) :: (Exp Float, Exp Float) in
            let (u_j,v_j) = unlift (uv ! index1 j) :: (Exp Float, Exp Float) in
            let (u_k,v_k) = unlift (uv ! index1 k) :: (Exp Float, Exp Float) in
            let du1 = u_j - u_i in
            let du2 = u_k - u_i in
            let dv1 = v_j - v_i in
            let dv2 = v_k - v_i in
            let det = 1 / ((du1 * dv2) - (du2 * dv1)) in
            lift (dv2/det, -du2/det, -dv1/det, du1/det) :: Exp (Float, Float, Float, Float)
    

getDefaultCloth :: (ParticleStates, AccVector (Float, Float), AccVector (Int, Int), AccVector (Int, Int, Int), AccMultiMatrix Float)
getDefaultCloth = (states, uv, edges, triangles, mass)
  where
    states = use $ fromList (Z :. 16 :. 3) $
              xyz [(0,0,0,0,0,0),
                  (1,0,0,0,0,0),
                  (2,0,0,0,0,0),
                  (3,0,0,0,0,0),
                  (0,1,0,0,0,0),
                  (1,1,0,0,0,0),
                  (2,1,0,0,0,0),
                  (3,1,0,0,0,0),
                  (0,2,0,0,0,0),
                  (1,2,0,0,0,0),
                  (2,2,0,0,0,0),
                  (3,2,0,0,0,0),
                  (0,3,0,0,0,0),
                  (1,3,0,0,0,0),
                  (2,3,0,0,0,0),
                  (3,3,0,0,0,0)] :: ParticleStates
    uv    = use $ fromList (Z :. 16)
                 [(0,0),
                  (1,0),
                  (2,0),
                  (3,0),
                  (0,1),
                  (1,1),
                  (2,1),
                  (3,1),
                  (0,2),
                  (1,2),
                  (2,2),
                  (3,2),
                  (0,3),
                  (1,3),
                  (2,3),
                  (3,3)] :: AccVector (Float, Float)
    edges = use $ fromList (Z :. 33)
            [(0,4),(0,1),
             (1,4),(1,5),(1,2),
             (2,5),(2,6),(2,3),
             (3,6),(3,7),
             (4,8),(4,5),
             (5,8),(5,9),(5,6),
             (6,9),(6,10),(6,7),
             (7,10),(7,11),
             (8,12),(8,9),
             (9,12),(9,13),(9,10),
             (10,13),(10,14),(10,11),
             (11,14),(11,15),
             (12,13),(13,14),(14,15)] :: AccVector (Int, Int)
    triangles = use $ fromList (Z :. 18) [(0,1,4),(1,5,4),(1,5,2),(2,6,5),(2,3,6),(3,7,6),
                 (4,5,8),(5,9,8),(5,6,9),(6,10,9),(6,7,10),(7,11,10),
                 (8,9,12),(9,13,12),(9,10,13),(10,14,13),(10,11,14),(11,15,14)] :: AccVector (Int,Int,Int)
    --
    newsize       = lift (Z :. 16 :. All :: Z :. Int :. All)
    mass_segments = replicate (newsize) $ use $ fromList (Z :. 3) [1,1,1] :: AccMatrix Int
    mass_indices  = replicate (newsize) $ use $ fromList (Z :. 3) [0,1,2] :: AccMatrix Int
    mass_values   = replicate (newsize) $ use $ fromList (Z :. 3) [1,1,1] :: AccMatrix Float
    mass_columns  = use $ fromList (Z :. 16) [3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3] :: AccVector Int
    mass          = (mass_segments, (mass_indices, mass_values), mass_columns) :: AccMultiMatrix Float
    
-- TODO Implement subtraction for SparseMatrices!
(|-|) a b = Acc.zipWith (-) a b
(|*|) a b = Acc.zipWith (*) a b


-- Stores (x,dx) (y,dy) (z,dz)
type ParticleStates = AccMatrix (Float, Float)

{-
-- | Calculates one step for the particles to go
step :: ParticleStates -> AccMatrix Int -> AccMatrix Int -> Int -> ParticleStates
step state edges triangles h' = state'
  where
    pfv      = replicate (shape m) $ unit 0 
    pfx      = replicate (shape m) $ unit 0 
    h        = replicate (shape pfv) $ unit h'
    h2       = replicate (shape pfv) $ unit (h' * h')
    m        = 
    a        = m |-| (h |*| pfv) |-| (h2 |*| pfx) -- TODO SparseMatrix!
    b        = 0
    p        = 
    z        = replicate (shape b) $ unit 0 -- TODO Really start with zero?
    dv       = mpcgMultiInitialAcc a b p 0.0000001 10 -- TODO Add Filter!
    state'   = Acc.map statemap $ Acc.zip state dv
    --
    statemap ((x,v),dv) = (x + h |*| (v |+| dv), v + dv)
-- -}

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
