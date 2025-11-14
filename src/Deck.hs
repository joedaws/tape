-- | Multiple cassette tapes in a deck
module Deck (tapeWidth) where

makeOdd :: Int -> Int
makeOdd n
  | n <= 0 = 1
  | even n = n - 1
  | otherwise = n

-- Total width
tapeWidth :: Int -> Int
tapeWidth windowWidth =
  makeOdd $ max 1 (windowWidth - labelWidth)
  where
    -- Rough estimate of label + spacing
    labelWidth = 10 -- tune this until it feels right
