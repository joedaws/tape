{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Event
  ( appEvent,
    Name (..),
    St (..),
    tapes,
    focusIdx,
    termHeight,
    statusMsg,
    modifyFocusedTape,
    addTapeToSt,
    focusNextSt,
    focusPrevSt,
    modifyFocusedTapeSt,
    calcMaxTapes,
    initialState,
    keyQuit,
    keyNextTape,
    keyPrevTape,
    keyAddTape,
    modAddTape,
  )
where

import qualified Brick.Main as M
import qualified Brick.Types as T
import Data.Char (isPrint)
import qualified Data.Text as DT
import Deck (tapeWidth)
import qualified Graphics.Vty as V
import Lens.Micro ((.~))
import Lens.Micro.Mtl
import Lens.Micro.TH
import Tape

newtype Name = Edit Int
  deriving (Ord, Show, Eq)

data St = St
  { _tapes      :: [Tape],
    _focusIdx   :: Int,
    _termHeight :: Int,
    _statusMsg  :: Maybe DT.Text
  }

makeLenses ''St

-- Key binding constants
keyQuit        :: V.Key;  keyQuit        = V.KEsc
keyNextTape    :: V.Key;  keyNextTape    = V.KChar '\t'
keyPrevTape    :: V.Key;  keyPrevTape    = V.KBackTab
keyAddTape     :: V.Key;  keyAddTape     = V.KChar 'n'
modAddTape     :: [V.Modifier]; modAddTape = [V.MCtrl]
keyBackspace   :: V.Key;  keyBackspace   = V.KBS
keyDelete      :: V.Key;  keyDelete      = V.KDel
keyCursorLeft  :: V.Key;  keyCursorLeft  = V.KLeft
keyCursorRight :: V.Key;  keyCursorRight = V.KRight

-- Screen capacity constants
rowsPerTape    :: Int; rowsPerTape    = 2
uiOverheadRows :: Int; uiOverheadRows = 3

calcMaxTapes :: Int -> Int
calcMaxTapes termH = max 1 ((termH - uiOverheadRows) `div` rowsPerTape)

-- Pure state helpers
addTapeToSt :: St -> St
addTapeToSt st
  | length (_tapes st) >= calcMaxTapes (_termHeight st) =
      st { _statusMsg = Just (DT.pack "No more vertical space for additional tapes.") }
  | otherwise =
      let tw = case _tapes st of
                 (t:_) -> _width t
                 []    -> 11
          newTape = (initTape "" 0) { _width = tw }
      in  st { _tapes     = _tapes st ++ [newTape],
               _focusIdx  = length (_tapes st),
               _statusMsg = Nothing }

focusNextSt :: St -> St
focusNextSt st =
  st { _focusIdx  = (_focusIdx st + 1) `mod` max 1 (length (_tapes st)),
       _statusMsg = Nothing }

focusPrevSt :: St -> St
focusPrevSt st =
  let n = length (_tapes st)
  in  st { _focusIdx  = (_focusIdx st - 1 + n) `mod` max 1 n,
           _statusMsg = Nothing }

modifyFocusedTapeSt :: (Tape -> Tape) -> St -> St
modifyFocusedTapeSt f st
  | idx < 0 || idx >= length ts = st
  | otherwise =
      let (before, tape : after) = splitAt idx ts
      in  st { _tapes = before ++ [f tape] ++ after }
  where
    idx = _focusIdx st
    ts  = _tapes st

-- Monadic wrapper
modifyFocusedTape :: (Tape -> Tape) -> T.EventM Name St ()
modifyFocusedTape f = T.modify (modifyFocusedTapeSt f)

initialState :: St
initialState = St
  { _tapes      = [initTape "" 0],
    _focusIdx   = 0,
    _termHeight = 24,
    _statusMsg  = Nothing
  }

appEvent :: T.BrickEvent Name e -> T.EventM Name St ()
appEvent (T.VtyEvent (V.EvKey k [])) | k == keyQuit        = M.halt
appEvent (T.VtyEvent (V.EvKey k [])) | k == keyNextTape    = T.modify focusNextSt
appEvent (T.VtyEvent (V.EvKey k [])) | k == keyPrevTape    = T.modify focusPrevSt
appEvent (T.VtyEvent (V.EvKey k ms)) | k == keyAddTape
                                     , ms == modAddTape    = T.modify addTapeToSt
appEvent (T.VtyEvent (V.EvKey k [])) | k == keyBackspace   = modifyFocusedTape backspace
appEvent (T.VtyEvent (V.EvKey k [])) | k == keyDelete      = modifyFocusedTape delete
appEvent (T.VtyEvent (V.EvKey k [])) | k == keyCursorLeft  = modifyFocusedTape rewind
appEvent (T.VtyEvent (V.EvKey k [])) | k == keyCursorRight = modifyFocusedTape forward
appEvent (T.VtyEvent (V.EvResize w h)) = do
  termHeight .= h
  let tw = tapeWidth w
  tapes %= fmap (width .~ tw)
appEvent (T.VtyEvent (V.EvKey (V.KChar c) []))
  | isPrint c = modifyFocusedTape (`insert` c)
appEvent _ = return ()
