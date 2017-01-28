module Test.Main where

import Prelude
import Data.ModularArithmetic
import Control.Monad.Eff.Console (logShow)
import Data.Typelevel.Num (D7)

mkZ7 :: Int -> Z D7
mkZ7 = mkZ

main = do
  logShow $ (mkZ7 4 + mkZ7 2)
  logShow $ (mkZ7 6 * mkZ7 5)
  logShow $ (mkZ7 2 - mkZ7 4)
  logShow $ (mkZ7 3 / mkZ7 4)
