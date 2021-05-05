module Golf where

takeEvery :: (Int, [a]) -> [a]
takeEvery (n, xs) = snd $ foldl (\(i, acc) curr -> (i + 1, if i `mod` n == 0 then acc ++ [curr] else acc)) (1, []) xs

skips :: [a] -> [[a]]
skips xs = map takeEvery (take (length xs) (zip [1..] $ repeat xs))

localMaxima :: [Integer] -> [Integer]
localMaxima (prev:curr:next:ns) = if curr > prev && curr > next then curr : localMaxima (curr:next:ns) else localMaxima (curr:next:ns)
localMaxima _ = []

histogram :: [Integer] -> String
-- compute the counts [1,1,1,5] -> [0,3,0,0,0,1,0,0] < done
--
histogram ns = foldl (\acc curr -> acc ++ curr ++ "\n") "" stars ++ "==========\n0123456789\n"
    where counts = map countElem [0..9]
          countElem elem = foldr (\curr acc -> acc + if curr == elem then 1 else 0) 0 ns
          stars = (\indexedFrequencies -> reverse $ map (\frequency -> foldl (\acc c -> if c >= frequency 
              then acc ++ "*"  else acc ++ " ") "" indexedFrequencies) [1..maximum indexedFrequencies]) counts