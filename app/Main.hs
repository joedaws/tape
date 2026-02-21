{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Util (on)
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
  ( hBox,
    padLeft,
    txt,
    vBox,
    withAttr,
    Padding (..),
  )
import qualified Brick.Widgets.Edit as E
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as DT
import qualified Data.Text.IO as TIO
import Deck (tapeWidth)
import Event
  ( Name (..),
    St (..),
    appEvent,
    focusIdx,
    initialState,
    statusMsg,
    tapes,
    termHeight,
  )
import qualified Graphics.Vty as V
import Lens.Micro ((.~), (^.))
import Lens.Micro.Mtl
import Tape (Tape, printTape, tapeText, width)

drawUI :: St -> [T.Widget Name]
drawUI st = [C.center $ vBox $ tapeWidgets ++ [statusRow, txt " ", txt helpText]]
  where
    focused = st ^. focusIdx
    tapeWidgets = zipWith drawRow [0 ..] (st ^. tapes)
    drawRow i tape =
      let attr  = if i == focused then E.editFocusedAttr else E.editAttr
          label = DT.pack ("Tape " ++ show (i + 1) ++ ":")
      in  withAttr attr $ hBox [txt label, padLeft (Pad 1) (txt (printTape tape))]
    statusRow = maybe (txt " ") txt (st ^. statusMsg)

helpText :: DT.Text
helpText = "Tab: next  Shift+Tab: prev  Ctrl+N: new tape  Esc: quit"

focusColorMap :: A.AttrMap
focusColorMap =
  A.attrMap
    V.defAttr
    [ (E.editAttr, V.white `on` V.blue),
      (E.editFocusedAttr, V.black `on` V.yellow)
    ]

appCursor :: St -> [T.CursorLocation Name] -> Maybe (T.CursorLocation Name)
appCursor _ _ = Nothing

tapeApp :: M.App St e Name
tapeApp =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = appCursor,
      M.appHandleEvent = appEvent,
      M.appStartEvent = do
        vty <- M.getVtyHandle
        (w, h) <- liftIO $ V.displayBounds (V.outputIface vty)
        termHeight .= h
        tapes %= fmap (width .~ tapeWidth w),
      M.appAttrMap = const focusColorMap
    }

main :: IO ()
main = do
  st <- M.defaultMain tapeApp initialState
  mapM_ printTapeOutput (zip [1 ..] (st ^. tapes))

printTapeOutput :: (Int, Tape) -> IO ()
printTapeOutput (i, t) = do
  putStrLn $ "Words recorded to Tape " ++ show i ++ ":\n"
  TIO.putStrLn (tapeText t)
