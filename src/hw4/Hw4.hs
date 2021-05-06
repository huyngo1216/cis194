import Data.Char ( toLower, toUpper )

-- https://www.seas.upenn.edu/~cis194/spring13/hw/04-higher-order.pdf

-- Exercise 1

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr (\curr acc -> acc * (curr - 2)) 1 . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum .
    filter even .
    takeWhile (>1) .
    iterate (\n' -> if even n' then n' `div` 2 else 3 * n' + 1)

-- Exercise 2

data Tree a = Leaf
    | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf = 0
height (Node h _ _ _) = h

insert :: a -> Tree a -> Tree a
insert a Leaf = Node 0 Leaf a Leaf
insert a (Node _ Leaf root Leaf) = Node 1 (insert a Leaf) root Leaf
insert a (Node h left root Leaf) = Node h left root (insert a Leaf)
insert a (Node h Leaf root right) = Node h (insert a Leaf) root right
insert a (Node h left root right) =
    let (leftHeight, rightHeight) = (height left, height right)
    in if leftHeight > rightHeight then Node h left root (insert a right)
       else if leftHeight == rightHeight then Node (height right' + 1) left root right'
       else Node h (insert a left) root right
    where right' = insert a right

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

-- Exercise 3
xor :: [Bool] -> Bool
xor = odd . foldr (\curr acc -> if curr then acc + 1 else acc) 0

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a acc -> f a : acc) []

-- Exercise 4
-- generate list of exclusions, then filter out that list in generating primes
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = let exclusions = sundaramExclude n
                  in [2*x+1 | x <- [1..n], x `notElem` exclusions]

sundaramExclude :: Integer -> [Integer]
sundaramExclude n = [i + j + 2 * i * j | i <- [1..n], j <- [i..n], i + j + 2 * i * j <= n]