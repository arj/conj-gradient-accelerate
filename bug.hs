import Data.Array.Accelerate as Acc

type AccVector a = Acc (Vector a)
type AccSparseVector a = ((AccVector Int), (AccVector a))

getEntry :: (Elt a) => Exp Int -> a -> AccSparseVector a -> Exp a
getEntry i d (idx,val) = Acc.snd $ the $ Acc.foldAll f def xs
  where
    xs  = Acc.zip idx val
    def = constant (0 :: Int,d)
    f ack v = (i ==* Acc.fst v) ? (v, ack)
--    f ack v = (i ==* i) ? (v, ack) -- This fails!
--    f ack v = ack -- This works!
--    f ack v = v   -- This works!

vectorFromSparseVector :: (Elt a) => AccSparseVector a -> Int -> a -> AccVector a
vectorFromSparseVector sv@(idx,val) size d = Acc.map m def
  where
    def = use $ fromList (Z :. size) $ take size $ ([1,2..] :: [Int]) :: AccVector Int
    m i = getEntry i d sv 

------------

nobug = getEntry 1 (0.0 :: Float) (idx, val) 
  where
    idx = use $ fromList (Z :. (1 :: Int)) ([1] :: [Int])
    val = use $ fromList (Z :. (1 :: Int)) ([1] :: [Float])

-------------------------

bug = vectorFromSparseVector (idx, val) 1 $ (0.0 :: Float)
  where
    idx = use $ fromList (Z :. (1 :: Int)) ([1] :: [Int])
    val = use $ fromList (Z :. (1 :: Int)) ([1] :: [Float])


