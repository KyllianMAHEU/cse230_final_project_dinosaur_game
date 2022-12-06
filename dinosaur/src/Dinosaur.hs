{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module Dinosaur where

import qualified Data.Map as M
import Data.Ratio ((%))
import Control.Monad.Random
import Control.Monad (guard)
import Data.Maybe (fromMaybe)
import Lens.Micro.TH (makeLenses)
import Lens.Micro ((&), (.~), (%~), (^.))
import Linear.V2 (V2(..))
import System.Random (Random(..), randomRs, newStdGen)
import qualified Data.Sequence as S
import Data.Sequence(ViewR(..), ViewL(..), viewr, viewl, (|>))
import Data.Monoid (Any(..), getAny)

-- Set differant game variables and types
data Game = Game
  { _dinosaur       :: Dinosaur       -- ^ dinosaur
  , _dir            :: Direction      -- ^ direction of left foot
  , _obstacles      :: S.Seq Obstacle -- ^ sequence of barriers on screen
  , _obsSizes       :: [Size]         -- ^ random barrier dimensions
  , _obsTypes       :: [ObsType]      -- ^ random barrier positions
  , _coin           :: Coord          -- ^ location of coin
  , _coins          :: Stream Coord   -- ^ list of random coin locations
--   , _level          :: Difficulty     -- ^ game's difficulty level
--   , _diffMap        :: DifficultyMap  -- ^ game's difficulty map
  , _dead           :: Bool           -- ^ game over flag
  , _paused         :: Bool           -- ^ paused flag
  , _scoreMod       :: Int            -- ^ controls how often we update the score
  , _score          :: Score          -- ^ score
  , _highscore      :: Score          -- ^ highscore of current sesh
  , _duckCountdown  :: Int            -- ^ countdown for standing up after ducking
  } deriving (Show)

type Coord = V2 Int             -- (x,y)

type Dinosaur = [Coord]         -- (x,y) of all points the dinosaur is in

data Stream a = a :| Stream a
    deriving (Show)

-- data Entity = Coin | Obs
--     deriving (Show)

type Score = Int                -- score is a number

type Obstacle = [Coord]         -- (x,y) of all points in obstacle

type Size = V2 Int              -- length and heigh of obs

data Direction = Up  | Down | Crouch | Still -- what direction dinosaur is going if at all
    deriving (Eq, Show)

data ObsType = Cactus | Bird   -- Type of the obstacle, either on the floor to jump over or flying to duck under
    deriving (Eq, Show)

makeLenses ''Game

-- Set game constants

width, height :: Int
width = 50
height = 10

crouchedDinosaur :: Dinosaur
crouchedDinosaur = [V2 10 0]

standingDinosaur :: Dinosaur
standingDinosaur = [V2 10 0, V2 10 1]

obsMinSize, obsMaxSize :: Int
obsMinSize = 1
obsMaxSize = 4

-- How many frames the crouching lasts
crouchTime :: Int
crouchTime = 10

-- Gameplay function defintions

-- Move coins and obstacles to the left every tick 
-- moveEntities :: TODO what is type of coin or obstacle?? 
moveEntities = fmap (+ V2 (-1) 0)

-- Remove coins and obstacles after they go off screen 
-- TODO add part for coins or make seperate methods for coin and obstacles
moveEntities :: Game -> Game
removeEntities g = 
    case view1 $ g^.obstacles of 
        EmptyL -> g
        a :< as -> let x = getEntityRight a in
            (if x <= 0 then g & barriers .~ as else g)


getDinosaurY :: Game -> Int
getDinosaurY g = 
    let dino = g^.dinosaur
        (V2 _ y) = haed dino
    in y

addObstacle :: Game -> Game
addObstacle g =
  let (p:ps) = g^.obsTypes
  in case p of
    Bird    -> addBird g & obsTypes .~ ps
    Cactus -> addCactus g & obsTypes .~ ps

addCactus :: Game -> Game
addCactus g =
  let (V2 w h:rest) = g^.obsSizes
      -- (DiffMod wm hm _) = getDiffMod g 
      newObs = createObstacle (V2 (min w wm) (min h hm)) 0
  in g & obstacles %~ (|> newObs) & obsSizes .~ rest

-- | Add random sky barrier (ypos is 1)
addBird :: Game -> Game
addBird g =
  let (V2 w h:rest) = g^.obsSizes
      -- (DiffMod wm hm _) = getDiffMod g 
      newObs = createObstacle (V2 (min w wm) (min h hm)) 1
  in g & obstacles %~ (|> newObs) & obsSizes .~ rest

createObstacle :: Size -> Int -> Obstacle
createObstacle (V2 w h) y =
  [V2 (width + a) (y + b) | a <- [0..w-1], b <- [0..h-1]]

inObstacles :: Coord -> S.Seq Obstacle -> Bool
inObstacles coord obs = getAny $ foldMap (Any . inObstacle coord) obs

inObstacle :: Coord -> Obstacle -> Bool
inObstacle coord obstacle = coord `elem` obstacle



