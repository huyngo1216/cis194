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
