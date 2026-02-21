{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Event
  ( appEvent,
    Name (..),
    St (..),
    focusRing,
    tape1,
    tape2,
    modifyFocusedTape,
  )
where

import qualified Brick.Focus as F
import qualified Brick.Main as M
import qualified Brick.Types as T
import Data.Char (isPrint)
import Deck
import qualified Graphics.Vty as V
import Lens.Micro.Mtl
import Lens.Micro.TH
import Tape

data Name
  = Edit1
  | Edit2
  deriving (Ord, Show, Eq)

data St = St
  { _focusRing :: F.FocusRing Name,
    _tape1 :: Tape,
    _tape2 :: Tape
  }

makeLenses ''St

modifyFocusedTape :: (Tape -> Tape) -> T.EventM Name St ()
modifyFocusedTape f = do
  r <- use focusRing
  case F.focusGetCurrent r of
    Just Edit1 -> tape1 %= f
    Just Edit2 -> tape2 %= f
    Nothing -> return ()

appEvent :: T.BrickEvent Name e -> T.EventM Name St ()
appEvent (T.VtyEvent (V.EvKey V.KEsc [])) =
  M.halt
appEvent (T.VtyEvent (V.EvKey (V.KChar '\t') [])) =
  focusRing %= F.focusNext
appEvent (T.VtyEvent (V.EvKey V.KBackTab [])) =
  focusRing %= F.focusPrev
-- window resize
appEvent (T.VtyEvent (V.EvResize w _h)) = do
  let tw = tapeWidth w
  tape1 . width .= tw
  tape2 . width .= tw

-- Backspace: delete char to the left
appEvent (T.VtyEvent (V.EvKey V.KBS [])) =
  modifyFocusedTape backspace
-- Delete: delete char to the right
appEvent (T.VtyEvent (V.EvKey V.KDel [])) =
  modifyFocusedTape delete
-- Cursor left/right:
appEvent (T.VtyEvent (V.EvKey V.KLeft [])) =
  modifyFocusedTape rewind
appEvent (T.VtyEvent (V.EvKey V.KRight [])) =
  modifyFocusedTape forward
-- Insert printable characters (except tab, which we use for focus)
appEvent (T.VtyEvent (V.EvKey (V.KChar c) []))
  | c /= '\t' && isPrint c =
      modifyFocusedTape (`insert` c)
-- Ignore all other events for now
appEvent _ = return ()
