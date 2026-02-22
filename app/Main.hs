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
  ( fill,
    hBox,
    str,
    txt,
    vBox,
    vLimit,
    withAttr,
  )
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
    reelRotation,
    statusMsg,
    tapes,
    termHeight,
    timerSecs,
    wordCountTape,
    wordGoal,
  )
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP
import Lens.Micro ((&), (.~), (^.))
import Lens.Micro.Mtl
import qualified Render
import System.Environment (getArgs)
import Tape (Tape, initTape, leftText, printTape, tapeText, width)

timerActiveAttr :: A.AttrName
timerActiveAttr = A.attrName "timerActive"

timerExpiredAttr :: A.AttrName
timerExpiredAttr = A.attrName "timerExpired"

hubFrames :: [Char]
hubFrames =
  [ '◓',
    '◐',
    '◒',
    '◑'
  ]

-- | Fill pattern string for a reel at a given fullness ratio (0.0 = empty, 1.0 = full).
reelPattern :: Double -> String
reelPattern r
  | r <= 0.00 = "·····"
  | r <= 0.25 = "·░░░·"
  | r <= 0.50 = "░░░░░"
  | r <= 0.75 = "▒▒▒▒▒"
  | otherwise = "▓▓▓▓▓"

-- | Cursor position ratio: 0.0 = start, 1.0 = end.
cursorRatio :: Tape -> Double
cursorRatio t =
  let l = DT.length (t ^. leftText)
      total = DT.length (tapeText t)
   in fromIntegral l / fromIntegral (max 1 total)

-- | Format the session stats content (timer and/or word goal).
formatStats :: St -> Int -> String
formatStats st totalWc =
  case (st ^. timerSecs, st ^. wordGoal) of
    (Nothing, Nothing) -> "◆"
    _ -> timerPart ++ sep ++ goalPart
  where
    timerPart = case st ^. timerSecs of
      Nothing -> ""
      Just n -> pad (n `div` 60) ++ ":" ++ pad (n `mod` 60)
    goalPart = case st ^. wordGoal of
      Nothing -> ""
      Just g -> show totalWc ++ " / " ++ show g
    sep = case (st ^. timerSecs, st ^. wordGoal) of
      (Just _, Just _) -> "  ·  "
      _ -> ""
    pad x = (if x < 10 then "0" else "") ++ show x

-- | Draw a single tape: top separator + text line.
drawTapeRow :: Bool -> Tape -> T.Widget Name
drawTapeRow isFocused tape =
  vBox
    [ vLimit 1 (fill (if isFocused then '═' else '─')),
      hBox [str " ", Render.renderTapeText [Render.edgeFadeEffect] isFocused (printTape tape), str " "]
    ]

-- | Draw the reel indicator + session stats bar.
drawReelStats :: St -> T.Widget Name
drawReelStats st =
  hBox [str lStr, C.hCenter statsWidget, str rStr]
  where
    ts = st ^. tapes
    focused = st ^. focusIdx
    tape = if null ts then initTape "" 0 else ts !! min focused (length ts - 1)
    ratio = cursorRatio tape
    hub = hubFrames !! (st ^. reelRotation `mod` length hubFrames)
    lStr = hub : reelPattern ratio
    rStr = reelPattern (1.0 - ratio) ++ [hub]
    totalWc = sum (map wordCountTape ts)
    content = formatStats st totalWc
    statsWidget = case st ^. timerSecs of
      Just 0 -> withAttr timerExpiredAttr (str content)
      Just _ -> withAttr timerActiveAttr (str content)
      Nothing -> str content

drawUI :: St -> [T.Widget Name]
drawUI st = [vBox $ tapeWidgets ++ [vLimit 1 (fill '─'), drawReelStats st, statusRow, txt helpText]]
  where
    focused = st ^. focusIdx
    tapeWidgets = zipWith (\i t -> drawTapeRow (i == focused) t) [0 ..] (st ^. tapes)
    statusRow = maybe (txt " ") txt (st ^. statusMsg)

helpText :: DT.Text
helpText = "Tab: next  Shift+Tab: prev  Ctrl+N: new tape  Esc: quit"

focusColorMap :: A.AttrMap
focusColorMap =
  A.attrMap
    V.defAttr
    [ (timerActiveAttr, V.green `on` V.black),
      (timerExpiredAttr, V.black `on` V.red)
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
    _ -> Nothing
parseTimerArg (_ : rest) = parseTimerArg rest
parseTimerArg [] = Nothing

parseWordGoalArg :: [String] -> Maybe Int
parseWordGoalArg ("-w" : n : _) =
  case reads n of
    [(goal, "")] | goal > 0 -> Just goal
    _ -> Nothing
parseWordGoalArg (_ : rest) = parseWordGoalArg rest
parseWordGoalArg [] = Nothing

main :: IO ()
main = do
  args <- getArgs
  let mSecs = parseTimerArg args
      mGoal = parseWordGoalArg args
  chan <- BC.newBChan 1
  case mSecs of
    Just _ -> void $ forkIO $ forever $ threadDelay 1000000 >> BC.writeBChan chan Tick
    Nothing -> return ()
  let initSt = initialState & timerSecs .~ mSecs & wordGoal .~ mGoal
  vty <- VCP.mkVty mempty
  st <- M.customMain vty (VCP.mkVty mempty) (Just chan) tapeApp initSt
  mapM_ printTapeOutput (zip [1 ..] (st ^. tapes))

printTapeOutput :: (Int, Tape) -> IO ()
printTapeOutput (i, t) = do
  putStrLn $ "Words recorded to Tape " ++ show i ++ ":\n"
  TIO.putStrLn (tapeText t)
