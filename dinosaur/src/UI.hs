{-# LANGUAGE OverloadedStrings #-}
module UI where

import Control.Monad (forever, void)
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
import System.Random(Random(..),)

-- data representation of time passage
data Tick = Tick 

type Name = ()

-- What each cell block of the game environment can be
data Cell = Dinosaur | Obstacle | Free  | Coin | SlowPwrUp
-- define app
app :: App Game Tick Name
app = App 
    {
        appDraw = drawUI,
        appChooseCursor = neverShowCursor,
        appHandleEvent = handleEvent,
        appStartEvent = return,
        appAttrMap = const theMap
    }

-- Event handler

-- CHANGED NEXT??
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
--handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) =
  --liftIO (writeIORef counter 0 >> initGame (g^.highscore)) >>= continue
handleEvent g _ = continue g

-- Draw the UI
drawUI :: Game -> [Widget Name]
drawUI g = [C.center $ padRight (Pad 2) (drawStats g) <+> drawGrid g]

drawStats :: Game -> Widget Name
drawStats g = hLimit 11
  $ vBox [ drawScore (g ^. score)
         , padTop (Pad 2) $ drawGameOver (g ^. dead)
         , padTop (Pad 6) $ drawHighScore (g ^. highscore)
         ]

drawScore :: Int -> Widget Name
drawScore n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str " score ")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

drawHighScore :: Int -> Widget Name
drawHighScore n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "highscore")
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
      | c `elem` g^. dinosaur          = Dinosaur
      | inObstacles c (g^.obstacles)   = Obstacle
      | inCoins c (g^.coins)           = Coin
      | inSlowPwrUps c (g^.slowPwrUps) = SlowPwrUp
      | otherwise                      = Free

drawCell :: Cell -> Widget Name
drawCell Dinosaur      = withAttr dinosaurAttr  cw
drawCell Obstacle   = withAttr obstacleAttr  cw
drawCell Coin      = withAttr coinAttr      cw
drawCell Free     = withAttr freeAttr     cw
drawCell SlowPwrUp = withAttr slowPwrUpAttr cw

cw :: Widget Name
cw = str "  "

theMap :: AttrMap 
theMap = attrMap V.defAttr
 [  (dinosaurAttr, V.green `on` V.green), 
    (obstacleAttr, V.red `on` V.red), 
    (coinAttr, V.yellow `on` V.yellow),
    (slowPwrUpAttr, V.blue `on` V.blue),
    (gameOverAttr, fg V.red `V.withStyle` V.bold)
 ]

dinosaurAttr, obstacleAttr, freeAttr, gameOverAttr, coinAttr, slowPwrUpAttr :: AttrName
dinosaurAttr  = "dinosaurAttr"
obstacleAttr  = "obstacleAttr"
freeAttr     = "freeAttr"
gameOverAttr  = "gameOver"
coinAttr      = "coinAttr"
slowPwrUpAttr = "slowDownAttr"


counter :: IORef Int
{-# NOINLINE counter #-}
counter = unsafePerformIO (newIORef 0)

playGame :: IO Game 
playGame = do
  chan <- newBChan 10
  forkIO $ forever $ do
    modifyIORef counter (+1)
    c' <- readIORef counter
    writeBChan chan Tick
    threadDelay (max (65000 - c' * 10) 35000)
 
  g <- initGame 0 
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder


  customMain initialVty (V.mkVty V.defaultConfig) (Just chan) app g

  -- customMain initialVty builder (Just chan) app g1



--main :: IO ()-
--main = do
  --  chan <- newBChan 10
    --forkIO $ forever $ do
      --  writeBChan chan Tick
        --threadDelay 100000
    --g <- initGame
    --let builder = V.mkVty V.defaultConfig
    --initialVty <- builder
    --customMain initialVty builder (Just chan) app g


--     chan <- newBChan 10
--     forkIO $ forever $ do
--     modifyIORef counter (+1)
--     c' <- readIORef counter
--     writeBChan chan Tick
--     threadDelay (max (65000 - c' * 10) 35000)
--     -- threadDelay 35000
--   g <- initGame 0
--   customMain (V.mkVty V.defaultConfig) (Just chan) app g