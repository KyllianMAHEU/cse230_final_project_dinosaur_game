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

type Score = Int                -- score is a number

type Obstacle = [Coord]         -- (x,y) of all points in obstacle

type Size = V2 Int              -- length and heigh of obs

data Direction = Up  | Down | Crouch | Still -- what direction dinosaur is going if at all
    deriving (Eq, Show)

data ObsType = Cactus | Bird   -- Type of the obstacle, either on the floor to jump over or flying to duck under
    deriving (Eq, Show)

makeLenses ''Game

-- Set game constants



