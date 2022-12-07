{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests where

import qualified Dinosaur           as Dino
import qualified Lib                as Lib
import qualified UI                 as UI

import Linear.V2 (V2(..))
import Test.QuickCheck
import Test.Tasty
import System.Exit

genV2 :: Gen (V2 Int)
genV2 = do
        x <- elements[0..40]
        y <- elements[0..20]
        return (V2 x y)

-- genV2List :: Gen (V2 Int) -> Gen [V2 Int]
-- genV2List g = do
--         len <- elements[0..8] :: Gen Int
--         listGenL len
        
--         where
--             -- listGenL :: Int -> 
--             listGenL 0 = []
--             listGenL n = v
--                 where 
--                     v = do
--                         x <- elements[0..40]
--                         y <- elements[0..20]
--                         return (V2 x y)
--                     rest <- listGenL (n-1)
--                     return (v:rest)
            
            


isCoinX :: (V2 Int) -> Bool
isCoinX v@(V2 x y) =  Dino.getCoinX v == x

isGetObstacleLeft :: [V2 Int] -> Bool
isGetObstacleLeft v@((V2 x y):vs) = Dino.getObstacleLeft(v) == x

isGetObstacleRight :: [V2 Int] -> Bool
isGetObstacleRight v = Dino.getObstacleRight v == let (V2 x _) = last v in x

-- Tests if coin x value is returned 
prop_getCoinX :: Property 
prop_getCoinX = forAll genV2 (isCoinX)

-- prop_GetObstacleLeft :: Property
-- prop_GetObstacleLeft = forAll (genV2List genV2) (isGetObstacleLeft)

-- prop_GetObstacleRight :: Property
-- prop_GetObstacleRight = forAll (genV2List genV2) (isGetObstacleRight)



return []
runTests = $quickCheckAll