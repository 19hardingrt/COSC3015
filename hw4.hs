--Problem 1a
data Pair1 a = Pair1 a a deriving Show
data Pair2 a b = Pair2 a b deriving Show

instance Functor Pair1 where
    fmap f (Pair1 a1 a2) = Pair1 (f a1) (f a2)

--Problem 1b
--COME BACK TO THIS
instance Functor (Pair2 a) where
    fmap f (Pair2 a b) = Pair2 a (f b)

--Problem 2a
data ITree a = ILeaf (Int -> a) | INode [ITree a]

appITree :: Int -> ITree a -> [a]
appITree _ = return []

--Problem 2b
instance Functor ITree where
  fmap f (ILeaf g)  = ILeaf (f . g)
  fmap f (INode ts) = INode ts'
    where
      ts' = map (fmap f) ts

--Problem 3
--This is the sequenceAL function renamed to kachow
--due to some weird compiler error
--wouldn't run with the sequenceAL name

kachow :: Applicative f => [f a] -> f [a]
kachow []     = pure []
kachow (x:xs) = pure (:) <*> x <*> kachow xs

--Problem 4a
newtype ST s a = S (s -> (a,s))




