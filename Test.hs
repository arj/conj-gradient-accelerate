import Data.Array.Accelerate as Acc
import Data.Array.Accelerate.CUDA as C
import Data.Array.Accelerate.Interpreter as I
import Data.Array.Accelerate.Math.SMVM as SMVM
import Data.Array.Accelerate.Types
import Cloth


smvmTest = smvmMulti (segd,(idxs,vals),cnts) vecs
  where
    segd = use $ fromList (Z:.2:.3) [2,1,1,0,2,1] :: Acc (Array DIM2 Int)
    idxs = use $ fromList (Z:.2:.4) [0,2,2,0,1,2,0,0] :: Acc (Array DIM2 Int)
    vals = use $ fromList (Z:.2:.4) [1,5,3,2,1,1,1,0] :: Acc (Array DIM2 Float)
    cnts = use $ fromList (Z:.2) [3,3] :: Acc (Array DIM1 Int)
    vecs = use $ fromList (Z:.2:.3) [1,2,3,4,5,6] :: Acc (Array DIM2 Float)


-- TESTS --

testmpcgMulti2 n = mpcgMultiInitialAcc a b z p epsilon n
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

testmpcgMulti3 :: Int -> Int -> AccMatrix Float
testmpcgMulti3 m n = mpcgMultiInitialAcc a b z p epsilon n
  where
    repit a = Acc.replicate (lift (Z :. m :. All :: (Z:.Int:.All))) a
    --
    allsegs = repit $ use $ fromList (Z :. 3) [3,3,3] :: AccMatrix Int
    allidxs = repit $ use $ fromList (Z :. 9) [0,1,2,0,1,2,0,1,2] :: AccMatrix Int
    allvals = repit $ use $ fromList (Z :. 9) [1.0,1.0,1.0,1.0,5.0,1.0,1.0,1.0,1.0] :: AccMatrix Float
    allcols = Acc.replicate (index1 $ constant m) $ unit $ constant 3 :: AccVector Int
    --
    a = (allsegs,(allidxs,allvals),allcols)
    b = repit $ use $ fromList (Z :. 3) [6,14,6] :: AccMatrix Float
    z = repit $ use $ fromList (Z :. 3) [0,0,0] :: AccMatrix Float
    p = repit $ use $ fromList (Z :. 3) [1,5,1] :: Acc (Array DIM2 Float)
    epsilon = 0.0000001

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
