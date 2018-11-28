module Test.Main where

import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Maybe
import Data.ModularArithmetic
import Data.ModularArithmetic.Primality as Primality
import Data.Ord (abs)
import Data.Typelevel.Num (class Pos, D1, D3, D5, D7, D8, D9, D11)
import Effect.Console (log, logShow)
import Prelude
import Test.QuickCheck (class Arbitrary, Result, arbitrary, quickCheck, (<?>))
import Test.QuickCheck.Gen (elements)
import Test.QuickCheck.Laws.Data (checkDivisionRing, checkCommutativeRing, checkEuclideanRing, checkRing, checkSemiring)
import Type.Proxy (Proxy(..))

newtype ArbZ m = ArbZ (Z m)

derive newtype instance eqArbZ :: Eq (ArbZ m)
derive newtype instance ordArbZ :: Ord (ArbZ m)
derive newtype instance showArbZ :: Show (ArbZ m)
derive newtype instance boundedArbZ :: Pos m => Bounded (ArbZ m)
derive newtype instance enumArbZ :: Pos m => Enum (ArbZ m)
derive newtype instance boundedEnumArbZ :: Pos m => BoundedEnum (ArbZ m)
derive newtype instance semiringArbZ :: Pos m => Semiring (ArbZ m)
derive newtype instance ringArbZ :: Pos m => Ring (ArbZ m)
derive newtype instance commutativeRingArbZ :: Pos m => CommutativeRing (ArbZ m)
derive newtype instance divisionRingArbZ :: Primality.Prime m => DivisionRing (ArbZ m)
derive newtype instance euclideanRingArbZ :: Primality.Prime m => EuclideanRing (ArbZ m)

instance arbitraryArbZ :: Pos m => Arbitrary (ArbZ m) where
  arbitrary = ArbZ <$> genZ

main = do
  checkPrime (Proxy :: Proxy D3)
  checkPrime (Proxy :: Proxy D5)
  checkPrime (Proxy :: Proxy D7)
  checkPrime (Proxy :: Proxy D11)

  checkComposite (Proxy :: Proxy D1)
  checkComposite (Proxy :: Proxy D8)
  checkComposite (Proxy :: Proxy D9)

  case reifyPrime 101 checkPrime' of
    Just go ->
      go
    Nothing -> do
      log "101 is not prime apparently."
      quickCheck false

-- | If m is composite, then Z m should be a commutative ring.
checkComposite :: forall m. Pos m => Proxy m -> _
checkComposite _ = do
  let p = Proxy :: Proxy (ArbZ m)

  log "Checking 'inverses' law"
  quickCheck (inverses :: ArbZ m -> _)
  checkSemiring p
  checkRing p
  checkCommutativeRing p

-- | If p is prime, then Z p should be a field.
checkPrime :: forall p. Prime p => Proxy p -> _
checkPrime _ = do
  let p = Proxy :: Proxy (ArbZ p)

  checkComposite (Proxy :: Proxy p)
  checkEuclideanRing p
  checkDivisionRing p

-- For use with reifyPrime
checkPrime' :: forall p. Prime p => p -> _
checkPrime' _ = checkPrime (Proxy :: Proxy p)

coprime :: Int -> Int -> Boolean
coprime a b = abs (gcd a b) == 1

inverses :: forall m. Pos m => ArbZ m -> Result
inverses (ArbZ x) =
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
