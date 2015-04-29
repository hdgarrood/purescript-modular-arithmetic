module Test.Main where

import Data.ModularArithmetic
import Debug.Trace

newtype Z10 = Z10 Number

runZ10 :: Z10 -> Number
runZ10 (Z10 x) = x

instance isNumberZ10 :: IsNumber Z10

instance modulusZ10 :: Modulus Z10 where
  modulus = const 10

main = do
  print $ runZ10 (Z10 4 + Z10 8)
  print $ runZ10 (Z10 7 * Z10 5)
  print $ runZ10 (Z10 2 - Z10 9)
