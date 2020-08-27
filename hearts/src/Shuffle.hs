module Shuffle where

import System.Random

-- shuffle a finite list
-- method: repeatedly draw one element at random position from the incoming list
shuffle :: [a] -> IO [a]
shuffle [] =
  return []
shuffle xs = do
  let l = length xs
  i <- randomRIO (0, l-1)
  let (xs1, x : xs2) = splitAt i xs
  pure (x:) <*> shuffle (xs2 ++ xs1)

-- shuffle several rounds
shuffleRounds :: Int -> [a] -> IO [a]
shuffleRounds n xs =
  if n <= 0
  then return xs
  else do
    ys <- shuffle xs
    shuffleRounds (n-1) ys

-- |distribute n-ways
distribute :: Int -> [a] -> [[a]]
distribute m xs = [ extract (drop i xs) | i <- [0 .. m-1]]
  where
    extract [] = []
    extract (x:xs) = x : extract (drop (m-1) xs)


