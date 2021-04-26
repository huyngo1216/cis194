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
