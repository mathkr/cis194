-- Homework 04

import Data.List (nub)

--------------
-- Exercise 1
--------------

-- The given functions, written in a non idiomatic way.
-- fun1 :: [Integer] -> Integer
-- fun1 [] = 1
-- fun1 (x:xs)
--  | even x    = (x - 2) * fun1 xs
--  | otherwise = fun1 xs
--
-- fun2 :: Integer -> Integer
-- fun2 1 = 0
-- fun2 n | even n    = n + fun2 (n `div` 2)
--        | otherwise = fun2 (3 * n + 1)

-- The rewritten functions, using 'wholemeal' programming practices.
fun1' :: [Integer] -> Integer
fun1' xs = product . map (\x -> x - 2) . filter even $ xs

fun2' :: Integer -> Integer
fun2' n = sum . filter even . takeWhile (> 1) $ iterate succ n
    where succ x = if even x then x `div` 2
                             else 3 * x + 1

--------------
-- Exercise 2
--------------

-- As I couldn't come up with a reasonably beautiful solution for this
-- exercise, I decided to go all in on the 'hackishness' front.

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Eq)

-- Directions showing where to insert into the tree
data Direction = L | R
    deriving (Show, Eq)

-- Generates directions for inserting in a balanced tree starting from the
-- given positions
balanced :: [[Direction]] -> [[Direction]]
balanced ds = next ++ balanced next
    where next          = ds >>= addPossible
          addPossible d = [d ++ [L], d ++ [R]]

-- Generate a balanced binary tree from a list of values using `foldr`.
foldTree :: [a] -> Tree a
foldTree xs = foldr insert Leaf . reverse $ xs `zip` ([] : balanced [[]])

-- Insert element into balanced binary tree
insert :: (a, [Direction]) -> Tree a -> Tree a
insert (x, dss@(L : ds)) (Node _ l y r) = Node (toInteger $ length dss) (insert (x, ds) l) y r
insert (x, dss@(R : ds)) (Node _ l y r) = Node (toInteger $ length dss) l y (insert (x, ds) r)
insert (x, _) _                         = Node 0 Leaf x Leaf

-- Printing a Tree in a readable manner for easier debugging
instance (Show a) => Show (Tree a) where
    show tree = show' tree 1

show' :: Show a => Tree a -> Int -> String
show' Leaf n           = "Leaf"
show' (Node d l x r) n = init . unlines $ ["(Node " ++ (show d),
                                            space n ++ show' l (n + 1),
                                            space n ++ show x,
                                            space n ++ show' r (n + 1),
                                            space (n - 1) ++ ")"]

space :: Int -> String
space n = concat $ replicate n "|  "

--------------
-- Exercise 3
--------------

xor :: [Bool] -> Bool
xor bs = foldr (\acc b -> acc == not b) False bs

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x a -> f x : a) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\y a -> a . (`f` y)) id xs base

--------------
-- Exercise 4
--------------

sieveSundaram :: Integer -> [Integer]
sieveSundaram n =  map ((+1) . (*2)) $ filter (not . (`elem` composites)) [1..n]
    where composites = filter (<= n) [i + j + 2 * i * j | i <- [1..n], j <- [1..n]]
