{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Util (on)
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
  ( str,
    vLimit,
    (<+>),
    (<=>),
    withAttr
  )
import qualified Brick.Widgets.Edit as E
import qualified Graphics.Vty as V
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Char (isPrint)

import Tape
import Deck

data Name
  = Edit1
  | Edit2
  deriving (Ord, Show, Eq)

data St =
    St { _focusRing :: F.FocusRing Name
       , _tape1     :: Tape
       , _tape2     :: Tape
       }

makeLenses ''St

drawUI :: St -> [T.Widget Name]
drawUI st = [ui]
  where
    currentFocus = F.focusGetCurrent (st ^. focusRing)

    drawTape :: Name -> Tape -> T.Widget Name
    drawTape nm tape =
      let base = str . T.unpack $ printTape tape
          attr = case currentFocus of
                   Just nm' | nm' == nm -> E.editFocusedAttr
                   _                    -> E.editAttr
      in withAttr attr base

    e1 = drawTape Edit1 (st ^. tape1)
    e2 = drawTape Edit2 (st ^. tape2)

    ui = C.center $
        (str "Tape 1:" <+> vLimit 5 e1) <=>
        str " " <=>
        (str "Tape 2:" <+> e2) <=>
        str " " <=>
        str "Press Tab to switch between tapes, Esc to quit."

modifyFocusedTape :: (Tape -> Tape) -> T.EventM Name St ()
modifyFocusedTape f = do
  r <- use focusRing
  case F.focusGetCurrent r of
    Just Edit1 -> tape1 %= f
    Just Edit2 -> tape2 %= f
    Nothing    -> return ()

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
  | c /= '\t' && isPrint c
  = modifyFocusedTape (`insert` c)

-- Ignore all other events for now
appEvent _ = return ()

initialState :: St
initialState =
    St (F.focusRing [Edit1, Edit2])
       (initTape "Tape 1" 3)
       (initTape "Tape 2" 0)

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (E.editAttr, V.white `on` V.blue),
      (E.editFocusedAttr, V.black `on` V.yellow)
    ]

appCursor :: St -> [T.CursorLocation Name] -> Maybe (T.CursorLocation Name)
appCursor _ _ = Nothing -- The printTape draws the cursor

tapeApp :: M.App St e Name
tapeApp =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = appCursor,
      M.appHandleEvent = appEvent,
      M.appStartEvent  = do
                vty <- M.getVtyHandle
                (windowWidth, _windowHeight) <- liftIO $ V.displayBounds (V.outputIface vty)
                let tw = tapeWidth windowWidth
                tape1 . width .= tw
                tape2 . width .= tw,
      M.appAttrMap = const theMap
    }

tapeContents :: Tape -> T.Text
tapeContents t = (t ^. leftText) `T.append` (t ^. rightText)

main :: IO ()
main = do
    st <- M.defaultMain tapeApp initialState
    putStrLn "Words recorded to Tape 1:\n"
    TIO.putStrLn $ tapeContents (st ^. tape1)
    putStrLn "Words recorded to Tape 2:\n"
    TIO.putStrLn $ tapeContents (st ^. tape2)
