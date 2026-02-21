{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Util (on)
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
  ( str,
    vLimit,
    withAttr,
    (<+>),
    (<=>),
  )
import qualified Brick.Widgets.Edit as E
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Deck
import Event (Name (..), St (..), appEvent, focusRing, tape1, tape2)
import qualified Graphics.Vty as V
import Lens.Micro
import Lens.Micro.Mtl
import Tape

drawUI :: St -> [T.Widget Name]
drawUI st = [ui]
  where
    currentFocus = F.focusGetCurrent (st ^. focusRing)

    drawTape :: Name -> Tape -> T.Widget Name
    drawTape nm tape =
      let base = str . T.unpack $ printTape tape
          attr = case currentFocus of
            Just nm' | nm' == nm -> E.editFocusedAttr
            _ -> E.editAttr
       in withAttr attr base

    e1 = drawTape Edit1 (st ^. tape1)
    e2 = drawTape Edit2 (st ^. tape2)

    ui =
      C.center $
        (str "Tape 1:" <+> vLimit 5 e1)
          <=> str " "
          <=> (str "Tape 2:" <+> e2)
          <=> str " "
          <=> str "Press Tab to switch between tapes, Esc to quit."

initialState :: St
initialState =
  St
    (F.focusRing [Edit1, Edit2])
    (initTape "Tape 1" 3)
    (initTape "Tape 2" 0)

focusColorMap :: A.AttrMap
focusColorMap =
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
      M.appStartEvent = do
        vty <- M.getVtyHandle
        (windowWidth, _windowHeight) <- liftIO $ V.displayBounds (V.outputIface vty)
        let tw = tapeWidth windowWidth
        tape1 . width .= tw
        tape2 . width .= tw,
      M.appAttrMap = const focusColorMap
    }

main :: IO ()
main = do
  st <- M.defaultMain tapeApp initialState
  putStrLn "Words recorded to Tape 1:\n"
  TIO.putStrLn $ tapeText (st ^. tape1)
  putStrLn "Words recorded to Tape 2:\n"
  TIO.putStrLn $ tapeText (st ^. tape2)
