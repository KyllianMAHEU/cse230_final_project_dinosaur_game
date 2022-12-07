{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Dinosaur           as Dino
import qualified Lib                as Lib
import qualified UI                 as UI

import Linear.V2 (V2(..))
import Test.QuickCheck
import Test.Tasty
import System.Exit

return []
runTests = $ quickCheckAll

main :: IO Bool
main = runTests

-- teststep ::  Score -> TestTree
-- teststep ts = 

-- Tests if coin x value is returned 
testgetCoinX :: Int -> Int -> Bool 
testgetCoinX x y = Dino.getCoinX(V2 (x) (y)) == x



--testcreateObstacle :: Bool
--testcreateObstacle = Dino.Obstacle((V2 (10) (10))) == Dino.createObstacle((V2 (10) (10)) 0)