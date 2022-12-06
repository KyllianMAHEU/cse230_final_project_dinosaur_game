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
<<<<<<< HEAD
--   , _coins          :: S.Seq Coord   -- ^ list of random coin locations
--   , _slowPwrUps     :: S.Seq Coord   -- ^ list of random slowdown power up locations
=======
  -- , _coin           :: Coord          -- ^ location of coin
  , _coins          :: S.Seq Coord   -- ^ list of random coin locations
  , _slowPwrUp      :: Coord          -- ^ location of slow-down power up
  , _cslowPwrUps    :: Stream Coord   -- ^ list of random slowdown power up locations
>>>>>>> 377bd19 (adding coin generation)
--   , _level          :: Difficulty     -- ^ game's difficulty level
--   , _diffMap        :: DifficultyMap  -- ^ game's difficulty map
  , _dead           :: Bool           -- ^ game over flag
  , _paused         :: Bool           -- ^ paused flag
  , _scoreMod       :: Int            -- ^ controls how often we update the score
  , _score          :: Score          -- ^ score
  , _highscore      :: Score          -- ^ highscore of current sesh
  , _crouchCount  :: Int              -- ^ countdown for standing up after ducking
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

type WidthMod = Int
type HeightMod = Int
type DistMod = [Int]

--data DiffMod = DiffMod
  --  {
    --    _widthmod :: WidthMod,
      --  _heightmod :: HeightMod,
      --  _distmod :: DistMod
    --} deriving (Eq, Show)

makeLenses ''Game

-- Set game constants

width, height :: Int
width = 50
height = 10

crouchedDinosaur :: Dinosaur
crouchedDinosaur = [V2 10 0]

standingDinosaur :: Dinosaur
standingDinosaur = [V2 10 0, V2 10 1]

maxDinosaurHeight :: Int
maxDinosaurHeight = 5

obsMinSize, obsMaxSize :: Int
obsMinSize = 1
obsMaxSize = 4

constScoreMod :: Int
constScoreMod = 4


-- How many frames the crouching lasts
crouchTime :: Int
crouchTime = 8

-- Gameplay function Definitions 

-- Dinosaur steps forward in the game
step :: Game -> Game
step g = fromMaybe g $ do
  guard $ not (g^.dead || g^.paused)
  return $ fromMaybe (gameStep g) (die g)

-- Checks if dinosaur is still or in "duck mode", if so changes to jump
jump :: Game -> Game
jump g = if g^.dir == Still || g^.dir == Crouch 
            then changeDir Up g 
            else g

crouch :: Game -> Game
crouch g = if g^.dir == Still || g^.dir == Down
            then changeDir Crouch g & crouchCount .~ crouchTime
            else g

-- Changes direction of the dinosaur 
changeDir :: Direction -> Game -> Game
changeDir d g = g & dir .~ d

-- | What to do if we are not dead.
gameStep :: Game -> Game
gameStep = setHighScore . incScore . move . generateObstacle . generateCoin
          removeObstacles . isStanding . adjustCrouchCount . collectCoin -- . powerUp

-- EASY TO CHANGE THESE CROUCH COUNTDOWN FUNCTIONALITY??
adjustCrouchCount :: Game -> Game
adjustCrouchCount = setDirFromCrouchCount . decreaseCrouchCount

setDirFromCrouchCount  :: Game -> Game
setDirFromCrouchCount g = if (g^.crouchCount <= 0) && (g^.dir == Crouch) then g & dir .~ Still else g

decreaseCrouchCount :: Game -> Game
decreaseCrouchCount g = if g^.crouchCount > 0 then g & crouchCount %~ subtract 1 else g

-- Check is dinosaur and coin are overlapping, if they are, then increment score to adjust
collectCoin :: Game -> Game
collectCoin g = if shouldCollectCoin then do
                      incScoreCoin g
                      nextCoin
                  else g

shouldCollectCoin :: Game -> Bool
shouldCollectCoin = let c = g^.coins
                        dino = g^.dinosaur
                      in getAny $ foldMap (Any . flip inCoins c) dino
  

-- Increment score based on the score modifier game constant value
incScore :: Game -> Game
incScore g = case g^.scoreMod of
  0 -> g & score %~ (+1) & scoreMod %~ incAndMod
  _ -> g & scoreMod %~ incAndMod
  where incAndMod x = (x + 1) `mod` constScoreMod

incScoreCoin :: Game -> Game
incScoreCoin g = g & score %~ (+50)

setHighScore :: Game -> Game
setHighScore g = if g^.score > g^.highscore
                   then g & highscore .~ (g^.score)
                   else g


die :: Game -> Maybe Game
die g = do
  guard $ dies g
  return $ g & dead .~ True

-- Check ALL obstacle collisions
dies :: Game -> Bool
dies g = let nextDino = nextDinosaur g
             nextObs = nextObsPoses g
          in getAny $ foldMap (Any . flip inObstacles nextObs) nextDino

-- get the next dinosaur position after movement
nextDinosaur :: Game -> Dinosaur
nextDinosaur g = moveDinosaur g^.dinosaur

-- get the next obstacle positions after moving
nextObsPoses :: Game -> S.Seq Obstacle
nextObsPoses g = moveObstacles g^.obstacles

isStanding :: Game -> Game
isStanding g = let d = g^.dir in
  case d of
    Crouch -> if isDinosaurOnGround g then g & dinosaur .~ crouchedDinosaur else g
    _    -> if isDinosaurOnGround g then g & dinosaur .~ standingDinosaur else g

setDir :: Direction -> Game -> Game
setDir d g = g & dir .~ d

-- Should the dinosaur stop moving in given direction
shouldDinosaurStop :: Direction -> Game -> Bool
shouldDinosaurStop d g = case d of
  Down -> isDinosaurOnGround g
  Crouch -> isDinosaurOnGround g
  Up   -> getDinosaurY g >= maxDinosaurHeight
  _    -> False

isDinosaurOnGround :: Game -> Bool
isDinosaurOnGround g = getDinosaurY g <= 0

getDinosaurY :: Game -> Int
getDinosaurY g = 
    let dino = g^.dinosaur
        (V2 _ y) = head dino
    in y


move :: Game -> Game
move = moveDinosaur . moveObstacles . moveCoins

moveCoinss :: Game -> Game
moveCoins g = g & coins %~ fmap moveCoin

moveCoin :: Coord -> Coord 
moveCoin = fmap (+ V2 (-1) 0)

moveDinosaur :: Game -> Game
moveDinosaur g = let d = g^.dir in
  case d of
    Up   -> if shouldDinosaurStop d g then setDir Down g else moveUpDown 1 g
    Down -> if shouldDinosaurStop d g then setDir Still g else
              (let gNext = moveUpDown (-1) g in
                if isDinosaurOnGround gNext then setDir Still gNext else gNext)
    Crouch -> if shouldDinosaurStop d g then g else moveUpDown (-1) g
    _    -> g

-- | Moves dino up or down
moveUpDown :: Int -> Game -> Game
moveUpDown amt g = g & dinosaur %~ fmap (+ V2 0 amt)

moveObstacles :: Game -> Game
moveObstacles g = g & obstacles %~ fmap moveObstacle

-- Move coins and obstacles to the left every tick 
moveObstacle :: Obstacle -> Obstacle -- TODO what is type of coin or obstacle??  CAn we move all at once
moveObstacle = fmap (+ V2 (-1) 0)

-- Remove coins and obstacles after they go off screen 
-- TODO add part for coins or make seperate methods for coin and obstacles
removeObstacles :: Game -> Game
removeObstacles g = 
    case viewl $ g^.obstacles of 
        EmptyL -> g
        a :< as -> let x = getObstacleRight a in
            (if x <= 0 then g & obstacles .~ as else g)


generateObstacle :: Game -> Game
generateObstacle g =
  -- let (DiffMod wm hm (d:ds)) = getDiffMod g
     -- newDiffMod = DiffMod wm hm ds
    case viewr $ g^.obstacles of
      EmptyR -> addObstacle g
      _ :> a -> let x = getObstacleLeft a in
                -- TODO: check back with the num below
                  if (width - x) > 20 then addObstacle g else g
                --  if (width - x) > d then setDiffMod newDiffMod (addObstacle g) else g

getObstacleLeft :: Obstacle -> Int
getObstacleLeft [] = 0
getObstacleLeft (V2 x _:_) = x

getObstacleRight :: Obstacle -> Int
getObstacleRight [] = width
getObstacleRight b = let (V2 x _) = last b in x


addObstacle :: Game -> Game
addObstacle g =
  let (t:ts) = g^.obsTypes
  in case t of
    Bird    -> addBird g & obsTypes .~ ts
    Cactus -> addCactus g & obsTypes .~ ts

addCactus :: Game -> Game
addCactus g =
  let (V2 w h:rest) = g^.obsSizes
      -- (DiffMod wm hm _) = getDiffMod g 
      newObs = createObstacle (V2 (w) (h)) 0
  in g & obstacles %~ (|> newObs) & obsSizes .~ rest

-- | Add random sky barrier (ypos is 1)
addBird :: Game -> Game
addBird g =
  let (V2 w h:rest) = g^.obsSizes
      -- (DiffMod wm hm _) = getDiffMod g 
      newObs = createObstacle (V2 (w) (h)) 1
  in g & obstacles %~ (|> newObs) & obsSizes .~ rest

createObstacle :: Size -> Int -> Obstacle
createObstacle (V2 w h) y =
  [V2 (width + a) (y + b) | a <- [0..w-1], b <- [0..h-1]]

inObstacles :: Coord -> S.Seq Obstacle -> Bool
inObstacles coord obs = getAny $ foldMap (Any . inObstacle coord) obs

inObstacle :: Coord -> Obstacle -> Bool
inObstacle coord obstacle = coord `elem` obstacle

-- Coin creation similar to obstacles
generateCoin :: Game -> Game
generateCoin g =
    case viewr $ g^.coins of
      EmptyR -> addCoin g
      _ :> a -> let x = getCoinX a in
                -- TODO: check back with the num below
                  if (width - x) > 40 then addCoin g else g

getCoinX :: Coord -> Int
getCoinX [] = 0
getCoinX (V2 x _) = x


addCoin :: Game -> Game
addCoin g =
  let (t:ts) = g^.obsTypes
  in case t of
    Bird    -> addCoinSky g & obsTypes .~ ts
    Cactus -> addCoinGround g & obsTypes .~ ts

addCoinGround :: Game -> Game
addCoinGround g =
  let -- (V2 w h:rest) = g^.obsSizes
      -- (DiffMod wm hm _) = getDiffMod g 
      newCoin = createCoin 0
  in g & coins %~ (|> newCoin) & obsSizes .~ rest

-- | Add random sky barrier (ypos is 1)
addCoinSky :: Game -> Game
addCoinSky g =
  let -- (V2 w h:rest) = g^.obsSizes
      -- (DiffMod wm hm _) = getDiffMod g

      newCoin = createCoin 2
  in g & coins %~ (|> newCoin) & obsSizes .~ rest

createCoin :: Int -> Coin
createObstacle y =
  [V2 (width) (y)]

inCoins :: Coord -> S.Seq Coord -> Bool
inCoins coord coins = getAny $ foldMap (Any . inCoin coord) coins

inCoin :: Coord -> Coord -> Bool
inCoin coin dino = coin `elem` dino


initGame :: Score -> IO Game
initGame hs = do
  sizes       <- randomRs (V2 obsMinSize obsMinSize, V2 obsMaxSize obsMaxSize) <$> newStdGen
  randomTypes <- flip weightedList ((Bird, 1 % 4) : replicate 3 (Cactus, 1 % 4)) <$> newStdGen
--   dMap            <- difficultyMap
  let g = Game { _dinosaur  = standingDinosaur
               , _dir       = Still
               , _obstacles = S.empty
               , _obsSizes  = sizes
               , _obsTypes   = randomTypes
               , _coins     = S.Empty
            --    , _level     = D0
            --    , _diffMap   = dMap
               , _paused    = False
               , _dead      = False
               , _scoreMod  = 0
               , _score     = 0
               , _highscore = hs
               , _crouchCount = -1 
    --           , _coins = S.empty
    --           , _slowPwrUps  = S.empty
               }
  return g


weightedList :: RandomGen g => g -> [(a, Rational)] -> [a]
weightedList gen weights = evalRand m gen
    where m = sequence . repeat . fromList $ weights


-- instance Random a => Random (V2 a) where
  -- randomR (V2 x1 y1, V2 x2 y2) g =
    -- let (x, g')  = randomR (x1, x2) g
       -- (y, g'') = randomR (y1, y2) g'
     -- in (V2 x y, g'')
--  random g =
   -- let (x, g')  = random g
   --     (y, g'') = random g'
    -- in (V2 x y, g'')

