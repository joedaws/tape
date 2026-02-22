-- | Multiple tapes in a deck
module Deck (tapeWidth, tapeRows) where

-- | Number of rows each tape widget occupies (separator + text)
tapeRows :: Int
tapeRows = 2

-- | Width of the tape text region (terminal width minus side padding)
tapeWidth :: Int -> Int
tapeWidth w = max 20 (w - 2)
