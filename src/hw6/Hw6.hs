fib :: Integer -> Integer
fib n
    | n == 0    = 0
    | n == 1    = 1
    | otherwise = fib (n - 1) + fib (n - 2)

-- Infinite fibonacci numbers
fibs1 :: [Integer]
fibs1 = [ fib i | i <- [0..] ]

-- https://stackoverflow.com/questions/26843529/haskell-fibonacci-explanation
-- https://stackoverflow.com/questions/6273621/understanding-a-recursively-defined-list-fibs-in-terms-of-zipwith
fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Streams
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a b) = a : streamToList b

-- Test instance `Show` via:
-- >>> take 20 $ streamToList (infiniteIntStream 1)`, par exemple.
infiniteIntStream :: Int -> Stream Int
infiniteIntStream i = Cons i (infiniteIntStream (i + 1))

instance Show a => Show (Stream a) where
    show a = show (streamToList a)

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

-- actually implementing a Functor instance for `Stream`
-- >>> take 20 $ streamToList $ streamMap (+1) $ (infiniteIntStream 0)
-- [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a b) = Cons (f a) (streamMap f b)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x (interleaveStreams ys xs)

-- I am not smart enough to see the pattern or how to use `interleaveStreams` but I will do the naive way for practice
-- 1. for every `n`, we'll get all the multiples of 2 up until `n`, but we can short-circuit if `n` is odd
-- 2. working in that list of multiples, we'll `mod` `n` with `multipleOf2` until we get the highest value of `multipleOf2` which satisfies `n` mod `multipleOf2` == 0
ruler :: Stream Integer
ruler = rulerHelper 1

gen2sUpTo :: Integer  -> [Integer]
gen2sUpTo n = [x | x <- [1..n], 2^x <= n]

rulerHelper :: Integer -> Stream Integer
rulerHelper n =
    if odd n then
        Cons 0 (rulerHelper (n + 1))
    else let twos = gen2sUpTo n in
        Cons (foldl (\acc curr -> if n `mod` (2^curr) == 0 then curr else acc) 0 twos) (rulerHelper (n + 1))
