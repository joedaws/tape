{-# LANGUAGE TemplateHaskell #-}

module Tape
  ( Tape(..),
    initTape,
    width,
    leftText,
    rightText,
    printTape,
    insert,
    backspace,
    delete,
    forward,
    rewind
  )
where

import Control.Lens
import Data.Text as T

cursorChar :: Char
cursorChar = '|'

-- Use | for cursor in rendering
-- adding:
-- When adding a character it comes after the |
-- implementation: add character to the end of left text, i.e. T.snoc
-- deleting:
-- When deleting a characer the character delete the character before |
-- implementatoin: delete character at the end of the left text if it exists

-- hel|lo it is me
-- (hel) + | + (o it is me)
-- leftText | rightText

data Tape = Tape
  { _width :: Int,
    _leftText :: T.Text,
    _rightText :: T.Text
  } deriving (Eq, Show)

makeLenses ''Tape

initTape :: T.Text -> Int -> Tape
initTape txt cursorInt = Tape{
  _width=defaultWidth,
  _leftText=leftText',
  _rightText=rightText'
               }
  where
    (leftText', rightText') = T.splitAt cursorInt txt

defaultWidth :: Int
defaultWidth = 11

-- n = 14
-- exmample: width = 10, cursorInt = 3, text = "hello it is me"
-- hello it is me
--    ^
-- 0123456789
--   hello it
-- exmample: width = 10, cursorInt = 7
-- hello it is me
--        ^
-- 0123456789
-- llo it is
-- exmample: width = 10, cursorInt = 12
-- hello it is me
--             ^
-- 0123456789
-- t is me
printTape :: Tape -> T.Text
printTape t = T.replicate spaceLeft space `T.append` trimmed `T.append` T.replicate spaceRight space
  where
    w = view width t
    space = T.pack " "
    middle = div w 2
    nCharLeft = T.length $ view leftText t
    spaceLeft = max 0 (middle - nCharLeft)
    nCharRight = T.length $ view rightText t
    spaceRight = max 0 (middle - nCharRight)
    takeLeftn = min middle nCharLeft
    takeRightn = min middle nCharRight
    trimmed = T.takeEnd takeLeftn (view leftText t) `T.append` T.cons cursorChar (T.take takeRightn (view rightText t))

insert :: Tape -> Char -> Tape
insert t toInsert = over leftText (`T.snoc` toInsert) t

backspace :: Tape -> Tape
backspace t
  | view leftText t == T.pack "" = t
  | otherwise = over leftText T.init t

delete :: Tape -> Tape
delete t
  | view rightText t == T.pack "" = t
  | otherwise = over rightText T.tail t

forward :: Tape -> Tape
forward t =
  case T.uncons (view rightText t) of
    Just (c, rest) ->
      let t'  = over leftText (`T.snoc` c) t
          t'' = set rightText rest t'
      in t''
    Nothing -> t

rewind :: Tape -> Tape
rewind t =
  case T.unsnoc (view leftText t) of
    Just (rest, c) ->
      let t'  = over rightText (T.cons c) t
          t'' = set leftText rest t'
      in t''
    Nothing -> t
