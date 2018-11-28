module Data.ModularArithmetic
  ( Z
  , mkZ
  , runZ
  , modulus
  , inverse
  , enumerate
  , genZ
  , module Primality
  ) where

import Prelude

import Data.ModularArithmetic.Primality (class Prime, isPrime, primeFactors, reifyPrime) as Primality

import Control.Monad.Gen (class MonadGen)
import Control.Monad.Gen as Gen
import Data.Array as Array
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Maybe (Maybe(..), fromJust)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Typelevel.Num (class Pos, toInt)
import Data.Typelevel.Undefined (undefined)
import Partial.Unsafe (unsafePartial)

-- | Integers modulo some positive integer m.
-- |
-- | The type argument should be a positive integer of the kind defined by [purescript-typelevel](https://pursuit.purescript.org/packages/purescript-typelevel).
-- | This way, the modulus that you're working with is specified in the type. Note
-- | that even though the modulus is captured at the type level, you can still use
-- | modulus values which are not known at compile time, with the [`reifyIntP`](https://pursuit.purescript.org/packages/purescript-typelevel/2.0.0/docs/Data.Typelevel.Num.Sets#v:reifyIntP) function.
-- |
-- | This type forms a commutative ring for any positive integer m, and
-- | additionally a field when m is prime. Unlike `Int` and `Number`, though,
-- | all of these instances are *fully law-abiding*.
-- |
-- | The runtime representation is identical to that of `Int`, except that
-- | values are guaranteed to be between 0 and m-1.
newtype Z m = Z Int

derive newtype instance eqZ :: Eq (Z m)
derive newtype instance ordZ :: Ord (Z m)
derive newtype instance showZ :: Show (Z m)

instance boundedZ :: Pos m => Bounded (Z m) where
  bottom = zero
  top    = Z $ toInt (undefined :: m) - 1

instance enumZ :: Pos m => Enum (Z m) where
  succ z = let z' = z + one
           in if z' > z then Just z' else Nothing
  pred z = let z' = z - one
           in if z' < z then Just z' else Nothing

instance boundedEnumZ :: Pos m => BoundedEnum (Z m) where
  cardinality = Cardinality (toInt (undefined :: m) - 1)
  toEnum x = let z = mkZ x
             in if runZ z == x then Just z else Nothing
  fromEnum = runZ

-- | Smart constructor for `Z` values.
mkZ :: forall m. Pos m => Int -> Z m
mkZ x =
  let
    m = toInt (undefined :: m)
  in
    -- This ensures that we get a number between 0 and m-1
    Z (((x `mod` m) + m) `mod` m)

-- | Get at the underlying `Int`.
runZ :: forall m. Z m -> Int
runZ (Z x) = x

instance semiringZ :: Pos m => Semiring (Z m) where
  add (Z x) (Z y) = mkZ (add x y)
  mul (Z x) (Z y) = mkZ (mul x y)
  zero = Z 0
  one = mkZ 1

instance ringZ :: Pos m => Ring (Z m) where
  sub (Z x) (Z y) = mkZ (sub x y)

instance commutativeRingZ :: Pos m => CommutativeRing (Z m)

instance divisionRingZ :: Primality.Prime m => DivisionRing (Z m) where
  -- The unsafePartial here is justifiable because
  --   a) recip is undefined when applied to 0, and
  --   b) the `Prime m` constraint ensures that m is coprime to any input.
  recip = unsafePartial (fromJust <<< inverse)

instance euclideanRingZ :: Primality.Prime m => EuclideanRing (Z m) where
  degree _ = 1
  div x y = x * recip y
  mod _ _ = Z 0

-- | Convenience function for accessing `m` at the value level.
modulus :: forall m. Pos m => Z m -> Int
modulus _ = toInt (undefined :: m)

-- | Compute a multiplicative inverse of some nonzero number in Z_m. Note that
-- | an inverse exists if and only if the input and `m` are coprime. If this is
-- | not the case, this function returns `Nothing`.
inverse :: forall m. Pos m => Z m -> Maybe (Z m)
inverse (Z a) = map mkZ (go 0 n 1 a)
  where
    n = toInt (undefined :: m)

    go t r _ 0 =
      if r > 1
        then Nothing
        else Just t
    go t r newt newr =
      let
        quot = r / newr
      in
        go newt newr (t - quot * newt) (r - quot * newr)

-- | List all members of Z_m.
enumerate :: forall m. Pos m => NonEmpty Array (Z m)
enumerate =
  let
    m = toInt (undefined :: m)
  in
    mkZ 0 :| (if m == 1 then [] else map mkZ (Array.range 1 (m - 1)))

-- | A random generator for elements of Z_m; selects any value of Z_m with
-- | each value being equally likely to be selected.
genZ :: forall gen m. MonadGen gen => Pos m => gen (Z m)
genZ = Gen.elements enumerate
