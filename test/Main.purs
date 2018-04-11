module Test.Main where

import Prelude
import Data.Ord (abs)
import Data.ModularArithmetic
import Data.Maybe
import Control.Monad.Eff.Console (log, logShow)
import Data.Typelevel.Num (class Pos, D1, D3, D5, D7, D8, D9, D11)
import Test.QuickCheck (class Arbitrary, Result, arbitrary, quickCheck, (<?>))
import Test.QuickCheck.Gen (elements)
import Test.QuickCheck.Laws.Data (checkCommutativeRing, checkEuclideanRing, checkField, checkRing, checkSemiring)
import Type.Proxy (Proxy(..))

main = do
  checkPrime (Proxy :: Proxy D3)
  checkPrime (Proxy :: Proxy D5)
  checkPrime (Proxy :: Proxy D7)
  checkPrime (Proxy :: Proxy D11)

  checkComposite (Proxy :: Proxy D1)
  checkComposite (Proxy :: Proxy D8)
  checkComposite (Proxy :: Proxy D9)

checkComposite :: forall m. Pos m => Proxy m -> _
checkComposite _ = do
  let p = Proxy :: Proxy (Z m)

  log "Checking 'inverses' law"
  quickCheck (inverses :: Z m -> _)
  checkSemiring p
  checkRing p
  checkCommutativeRing p

checkPrime :: forall p. Prime p => Proxy p -> _
checkPrime _ = do
  let p = Proxy :: Proxy (Z p)

  checkComposite (Proxy :: Proxy p)
  checkEuclideanRing p
  checkField p

coprime :: Int -> Int -> Boolean
coprime a b = abs (gcd a b) == 1

inverses :: forall m. Pos m => Z m -> Result
inverses x =
  case inverse x of
    Just y ->
      x * y == one
        <?> ("inverse did not multiply to one; x^{-1}: " <> show y
            <> ", " <> info)
    Nothing ->
      not (coprime (runZ x) m)
        <?> ("inverse returned Nothing but x, m were coprime. " <> info)

  where
  info = "x: Z " <> show (runZ x) <> ", m: " <> show m
  m = modulus x
