{-
    https://www.seas.upenn.edu/~cis194/spring13/hw/01-intro.pdf
    Validating Credit Card Numbers
-}

-- Exercise1
toDigits :: Integer -> [Integer]
toDigits x = let quotient = div x 10
                in (
                    if quotient <= 0 then
                        if x <= 0 then [] else [x]
                    else toDigits quotient ++ [mod x 10]
                )

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse' $ toDigits x
    where reverse' (a:as) = reverse as ++ [a]
          reverse' [] = []

-- Exercise2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = fst $
    foldr (\curr (acc, include) ->
        if include then (curr * 2 : acc, False)
        else (curr : acc, True)
    ) ([] , False) xs

-- Exercise3
sumDigits :: [Integer] -> Integer
sumDigits ns = foldr (\acc curr -> curr + (sum $ toDigits acc)) 0 ns

-- Exercise4
validate :: Integer -> Bool
validate n = mod (sumDigits . doubleEveryOther . toDigits $ n) 10 == 0


{-
    https://www.seas.upenn.edu/~cis194/spring13/hw/01-intro.pdf
    The Towers of Hanoi
-}
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n source target aux = if n > 0
    then hanoi (n - 1) source aux target ++ [(source, target)] ++ hanoi (n - 1) aux target source
    else []
