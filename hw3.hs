{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
--Problem 1
oscMap :: (a -> b) -> (a -> b) -> [a] -> [b]
oscMap p q [] = []
oscMap p q [x] = [p x]
oscMap p q (x : y : xs) = p x : q y : oscMap p q xs

--Problem 2
data  List a = Nil | Cons a (List a) deriving  Show

app :: List a -> List a -> List a
app Nil     ys = ys
app (Cons x xs) ys = Cons x (app xs ys)

--Problem 3
list2int :: [Int] -> Int
list2int = foldr (\x acc -> acc*10 + x) 0
--Implement iterate and reverse? 

--Problem 4a
data Tree a = Leaf | Node a (Tree a) (Tree a) deriving Show

size :: Tree a -> Int
size Leaf = 0
size (Node x l r) = 1 + size l + size r

--Problem 4b
insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node x Leaf Leaf
insert x node@(Node y l r)
    | x < y = Node y (insert x l) r
    | x > y = Node y l (insert x r)
    | otherwise = node


--Problem 4c
squash :: Tree a -> [a]
squash Leaf = []
squash (Node a l r) = (squash l) ++ [a] ++ (squash r)


--Problem 4d
unsquash :: Ord a => [a] -> Tree a
unsquash = foldr to_tree Leaf
      where
        to_tree :: Ord a => a -> Tree a -> Tree a
        to_tree x Leaf                     = Node x Leaf Leaf
        to_tree x (Node y l r) = if x <= y then Node y (to_tree x l) r
                                   else Node y l (to_tree x r)

--problem 4e 




--problem 4f
foldt :: (a -> b -> b -> b) -> b -> Tree a -> b
foldt f x Leaf = x
foldt f x (Node val l r) = f val (foldt f x l) (foldt f x r)
foldt f x t = undefined


--Completely Stuck

--Problem 4g
treesum :: Num a => Tree a -> a
treesum Leaf = 0
treesum (Node x l r) = x + treesum l + treesum r

--Completely Stuck


--Problem 4h
--Don't fully understand question being asked.

--Problem 4i
depth :: Tree a -> Int
depth Leaf = 0
depth (Node _ t1 t2) = 1 + max (depth t1) (depth t2)

