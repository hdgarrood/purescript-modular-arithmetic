module Test.Main where

import Prelude
import Data.ModularArithmetic
import Control.Monad.Eff.Console (logShow)
import Data.Typelevel.Num (D11)
import Test.QuickCheck (class Arbitrary, Result, arbitrary, quickCheck, (<?>))
import Test.QuickCheck.Gen (elements)
import Test.QuickCheck.Laws.Data (checkCommutativeRing, checkEuclideanRing, checkField, checkRing, checkSemiring)
import Type.Proxy (Proxy(..))

type Z11 = Z D11

main = do
  let p = Proxy :: Proxy Z11

  quickCheck (inverses :: Z11 -> _)

  checkSemiring p
  checkRing p
  checkCommutativeRing p
  checkEuclideanRing p
  checkField p

inverses :: forall m. (Prime m) => Z m -> Result
inverses x = go <?> "x: " <> show x
  where
    go
      | x /= zero =
          x * inverse x == one
      | otherwise =
          true
