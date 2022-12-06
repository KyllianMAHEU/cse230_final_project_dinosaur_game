{-# LANGUAGE OverloadedStrings #-}
module UI where

import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

import Dinosaur

import Brick
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border as B
import Brick.Widgets.Border.Style as BS
import Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Linear.V2 (V2(..))
import Lens.Micro ((^.))

-- data representation of time passage
data Tick = Tick 

type Name = ()

-- What each cell block of the game environment can be
data Cell = Dinosaur | Obstacle | Coin | SlowPwrUp | Free

-- define app
app :: App UI Tick Name
app = App 
    {
        appDraw = drawUI,
        appChooseCursor = neverShowCursor,
        appHandleEvent = handleEvent,
        appStartEvent = return,
        appAttrMap = const theMap
    }

-- Event handler
handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                       = continue $ step g
-- handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ handleUp g
-- handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ handleDown g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'w') [])) = continue $ jump g
handleEvent g (VtyEvent (V.EvKey (V.KChar 's') [])) = continue $ crouch g
-- handleEvent g (VtyEvent (V.EvKey (V.KChar 'p') [])) = continue $ pause g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) =
  liftIO (writeIORef counter 0 >> initGame (g^.highscore)) >>= continue
handleEvent g _ = continue g

-- Draw the UI
drawUI :: Game -> [Widget Name]
drawUI g = [C.center $ padRight (Pad 2) (drawStats g) <+> drawGrid g]

drawStats :: Game -> Widget Name
drawStats g = hLimit 11
  $ vBox [ drawScore (g ^. score)
         , padTop (Pad 2) $ drawGameOver (g ^. dead)
         ]

drawScore :: Int -> Widget Name
drawScore n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str " score ")
  $ C.hCenter
  $ padAll 1
  $ str $ show n


drawGameOver :: Bool -> Widget Name
drawGameOver isDead =
  if isDead
     then withAttr gameOverAttr $ C.hCenter $ str "game over"
     else emptyWidget

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Primitive Chrome Dinosaur Game")
  $ vBox rows
  where
    rows = [hBox $ cellsInRow r | r <- [height - 1,height - 2..0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..width - 1]]
    drawCoord = drawCell . cellAt
    cellAt c
      | c `elem` g^. dinosaur        = Dinosaur
      | inObstacles c (g^.obstacles) = Obstacle
      | c == g ^. coin               = Coin
      | c == g ^. slowPwrUp          = SlowPwrUp
      | otherwise                    = Empty

drawCell :: Cell -> Widget Name
drawCell Dino      = withAttr dinosaurAttr  cw
drawCell Barrier   = withAttr obstacleAttr  cw
drawCell Coin      = withAttr coinAttr      cw
drawCell Empty     = withAttr emptyAttr     cw
drawCell SlowPwrUp = withAttr slowPwrUpAttr cw

cw :: Widget Name
cw = str "  "

theMap :: AttrMap 
theMap = attrMap V.defAttr
 [ (dinosaurAttr, V.white `on` V.white), 
    (obstacleAttr, V.red `on` V.red), 
    (coinAttr, V.yellow `on` V.yellow),
    (slowPwrUpAttr, V.blue `on` V.blue),
    (gameOverAttr, fg V.red `V.withStyle` V.bold)
 ]

dinosaurAttr, obstacleAttr, emptyAttr, gameOverAttr :: AttrName
dinosaurAttr  = "dinosaurAttr"
obstacleAttr  = "obstacleAttr"
emptyAttr     = "emptyAttr"
gameOverAttr  = "gameOver"
slowPwrUpAttr = "slowDownAttr"

main :: IO ()
main = do
    chan <- newBChan 10
    forkIO $ forever $ do
        writeBChan chan Tick
        threadDelay 100000
    g <- initGame
    let builder = V.mkVty V.defaultConfig
    initialVty <- builder
    void $ customMain initialVty builder (Just chan) app g


--     chan <- newBChan 10
--     forkIO $ forever $ do
--     modifyIORef counter (+1)
--     c' <- readIORef counter
--     writeBChan chan Tick
--     threadDelay (max (65000 - c' * 10) 35000)
--     -- threadDelay 35000
--   g <- initGame 0
--   customMain (V.mkVty V.defaultConfig) (Just chan) app g