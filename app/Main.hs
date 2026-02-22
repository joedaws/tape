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
    str,
    txt,
    vBox,
    withAttr,
  )
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as DT
import qualified Data.Text.IO as TIO
import Deck (tapeWidth, tapeWindowWidth)
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
import Tape (Tape, leftText, printTape, tapeText, width)

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

-- Another option for hubFrames
--  [ '⨂',
--   '⨁'
--  ]

-- | Fill pattern for a reel at a given fullness ratio (0.0 = empty, 1.0 = full).
-- Returns (topRow5, hubEndChar, bottomRow5).
reelPattern :: Double -> (String, Char, String)
reelPattern r
  | r <= 0.00 = ("·····", '·', "·····")
  | r <= 0.25 = ("·░░░·", '·', "·░░░·")
  | r <= 0.50 = ("░░░░░", '░', "░░░░░")
  | r <= 0.75 = ("▒▒▒▒▒", '▒', "▒▒▒▒▒")
  | otherwise = ("▓▓▓▓▓", '▓', "▓▓▓▓▓")

-- | Build the hub row: [hubEnd, spokeL, '○', spokeR, hubEnd]
buildHubRow :: Char -> Char -> String
buildHubRow e s = [e, ' ', s, ' ', e]

-- | Cursor position ratio: 0.0 = start, 1.0 = end.
cursorRatio :: Tape -> Double
cursorRatio t =
  let l = DT.length (t ^. leftText)
      total = DT.length (tapeText t)
   in fromIntegral l / fromIntegral (max 1 total)

-- | Format the 37-char stats string for the tape window row.
formatStats :: St -> Int -> String
formatStats st wc =
  case (st ^. timerSecs, st ^. wordGoal) of
    (Nothing, Nothing) -> replicate 17 '─' ++ " ◆ " ++ replicate 17 '─'
    _ ->
      let timerPart = case st ^. timerSecs of
            Nothing -> ""
            Just n -> pad (n `div` 60) ++ ":" ++ pad (n `mod` 60)
          goalPart = case st ^. wordGoal of
            Nothing -> ""
            Just g -> show wc ++ " / " ++ show g
          sep = case (st ^. timerSecs, st ^. wordGoal) of
            (Just _, Just _) -> "  ·  "
            _ -> ""
          content = timerPart ++ sep ++ goalPart
          padTotal = tapeWindowWidth - length content
          padLeft = padTotal `div` 2
          padRight = padTotal - padLeft
       in if padTotal < 0
            then take tapeWindowWidth content
            else replicate padLeft ' ' ++ content ++ replicate padRight ' '
  where
    pad x = (if x < 10 then "0" else "") ++ show x

drawCassette :: Bool -> St -> Int -> Tape -> T.Widget Name
drawCassette isFocused st idx tape =
  vBox
    [ str row1,
      str row2,
      str row3,
      str row4,
      str row5,
      str row6,
      str row7,
      row8Widget,
      row9Widget,
      str row10,
      str row11
    ]
  where
    hub = hubFrames !! (st ^. reelRotation `mod` length hubFrames)
    ratio = cursorRatio tape
    (lTop, lEnd, lBot) = reelPattern ratio
    (rTop, rEnd, rBot) = reelPattern (1.0 - ratio)
    lHub = buildHubRow lEnd hub
    rHub = buildHubRow rEnd hub
    wc = wordCountTape tape

    row1 = "╭" ++ replicate 61 '─' ++ "╮"
    row2 = "│ ┌" ++ replicate 55 '─' ++ "┐   │"
    row3 = "│ │  ♩  " ++ take 50 ("Tape " ++ show (idx + 1) ++ replicate 50 ' ') ++ "│   │"
    row4 = "│ └" ++ replicate 55 '─' ++ "┘   │"
    row5 = "│" ++ replicate 61 ' ' ++ "│"
    row6 = "│ ╭─────╮" ++ replicate 43 ' ' ++ "╭─────╮   │"
    row7 = "│ │" ++ lTop ++ "├──╔" ++ replicate 37 '═' ++ "╗──┤" ++ rTop ++ "│   │"
    row10 = "│ ╰─────╯  ╚" ++ replicate 37 '═' ++ "╝  ╰─────╯   │"
    row11 = "╰" ++ replicate 61 '─' ++ "╯"

    statsStr = formatStats st wc
    statsWidget = case st ^. timerSecs of
      Just 0 -> withAttr timerExpiredAttr (str statsStr)
      Just _ -> withAttr timerActiveAttr (str statsStr)
      Nothing -> str statsStr

    row8Widget =
      hBox
        [ str ("│ │" ++ lHub ++ "│  ║"),
          statsWidget,
          str ("║  │" ++ rHub ++ "│   │")
        ]

    row9Widget =
      hBox
        [ str ("│ │" ++ lBot ++ "├──║"),
          Render.renderTapeText [Render.edgeFadeEffect] isFocused (printTape tape),
          str ("║──┤" ++ rBot ++ "│   │")
        ]

drawUI :: St -> [T.Widget Name]
drawUI st = [C.center $ vBox $ tapeWidgets ++ [statusRow, txt " ", txt helpText]]
  where
    focused = st ^. focusIdx
    tapeWidgets = zipWith (\i t -> drawCassette (i == focused) st i t) [0 ..] (st ^. tapes)
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
