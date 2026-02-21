{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import qualified Brick.AttrMap as A
import qualified Brick.BChan as BC
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
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as DT
import qualified Data.Text.IO as TIO
import Deck (tapeWidth)
import Event
  ( AppEvent (..),
    Name (..),
    St (..),
    appEvent,
    focusIdx,
    initialState,
    statusMsg,
    tapes,
    termHeight,
    timerSecs,
  )
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP
import Lens.Micro ((&), (.~), (^.))
import Lens.Micro.Mtl
import System.Environment (getArgs)
import Tape (Tape, printTape, tapeText, width)

timerActiveAttr :: A.AttrName
timerActiveAttr = A.attrName "timerActive"

timerExpiredAttr :: A.AttrName
timerExpiredAttr = A.attrName "timerExpired"

drawUI :: St -> [T.Widget Name]
drawUI st = [C.center $ vBox $ timerRow ++ tapeWidgets ++ [statusRow, txt " ", txt helpText]]
  where
    focused     = st ^. focusIdx
    tapeWidgets = zipWith drawRow [0 ..] (st ^. tapes)
    drawRow i tape =
      let attr  = if i == focused then E.editFocusedAttr else E.editAttr
          label = DT.pack ("Tape " ++ show (i + 1) ++ ":")
      in  withAttr attr $ hBox [txt label, padLeft (Pad 1) (txt (printTape tape))]
    statusRow = maybe (txt " ") txt (st ^. statusMsg)
    timerRow  = case st ^. timerSecs of
      Nothing -> []
      Just n  ->
        let attr    = if n == 0 then timerExpiredAttr else timerActiveAttr
            pad x   = (if x < 10 then "0" else "") ++ show x
            label   = DT.pack (pad (n `div` 60) ++ ":" ++ pad (n `mod` 60))
            display = if n == 0 then "Time's up!" else "Timer: " <> label
        in  [withAttr attr (txt display)]

helpText :: DT.Text
helpText = "Tab: next  Shift+Tab: prev  Ctrl+N: new tape  Esc: quit"

focusColorMap :: A.AttrMap
focusColorMap =
  A.attrMap
    V.defAttr
    [ (E.editAttr,        V.white `on` V.blue),
      (E.editFocusedAttr, V.black `on` V.yellow),
      (timerActiveAttr,   V.green `on` V.black),
      (timerExpiredAttr,  V.black `on` V.red)
    ]

appCursor :: St -> [T.CursorLocation Name] -> Maybe (T.CursorLocation Name)
appCursor _ _ = Nothing

tapeApp :: M.App St AppEvent Name
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

parseTimerArg :: [String] -> Maybe Int
parseTimerArg ("-t" : n : _) =
  case reads n of
    [(mins, "")] | mins > 0 -> Just (mins * 60)
    _                       -> Nothing
parseTimerArg (_ : rest) = parseTimerArg rest
parseTimerArg []          = Nothing

main :: IO ()
main = do
  args   <- getArgs
  let mSecs = parseTimerArg args
  chan   <- BC.newBChan 1
  case mSecs of
    Just _  -> void $ forkIO $ forever $ threadDelay 1000000 >> BC.writeBChan chan Tick
    Nothing -> return ()
  let initSt = initialState & timerSecs .~ mSecs
  vty <- VCP.mkVty mempty
  st <- M.customMain vty (VCP.mkVty mempty) (Just chan) tapeApp initSt
  mapM_ printTapeOutput (zip [1 ..] (st ^. tapes))

printTapeOutput :: (Int, Tape) -> IO ()
printTapeOutput (i, t) = do
  putStrLn $ "Words recorded to Tape " ++ show i ++ ":\n"
  TIO.putStrLn (tapeText t)
