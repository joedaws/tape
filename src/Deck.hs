-- | Multiple cassette tapes in a deck
module Deck (tapeWidth, tapeWindowWidth, cassetteRows) where

-- | Fixed width of the tape text window (must be odd for cursor centering)
tapeWindowWidth :: Int
tapeWindowWidth = 37

-- | Number of rows each cassette widget occupies
cassetteRows :: Int
cassetteRows = 11

-- | Width of the tape text region (argument ignored; kept for API compat)
tapeWidth :: Int -> Int
tapeWidth _ = tapeWindowWidth
