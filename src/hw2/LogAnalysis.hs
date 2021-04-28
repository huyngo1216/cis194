-- HW2, ADTs https://www.seas.upenn.edu/~cis194/spring13/hw/02-ADTs.pdf
{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
import Prelude

parseMessage :: String -> LogMessage
parseMessage s = case words s of
                    "E":sev:t:m -> LogMessage (Error (read sev :: Int)) (read t :: Int) (unwords m)
                    "W":t:m -> LogMessage Warning (read t :: Int) (unwords m)
                    "I":t:m -> LogMessage Info (read t :: Int) (unwords m)
                    _ -> Unknown s

parse :: String -> [LogMessage]
parse contents = fmap parseMessage (lines contents)