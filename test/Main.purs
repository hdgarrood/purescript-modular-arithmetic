module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Data.ModularArithmetic (class Prime, Z, genZ, inverse)
import Data.Typelevel.Num (D11)
import Test.QuickCheck (Result, quickCheck, (<?>))
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws.Data (checkCommutativeRingGen, checkEuclideanRingGen, checkFieldGen, checkRingGen, checkSemiringGen)

type Z11 = Z D11

main
  :: forall eff
   . Eff
     ( console :: CONSOLE
     , random :: RANDOM
     , exception :: EXCEPTION
     | eff )
     Unit
main = do
  let gen = genZ :: Gen Z11

  quickCheck $ (inverses :: Z11 -> Result) <$> gen

  checkSemiringGen gen
  checkRingGen gen
  checkCommutativeRingGen gen
  checkEuclideanRingGen gen
  checkFieldGen gen

inverses :: forall m. Prime m => Z m -> Result
inverses x = go <?> "x: " <> show x
  where
    go
      | x /= zero =
          x * inverse x == one
      | otherwise =
          true
