module Data.ModularArithmetic
  ( Z
  , mkZ
  , runZ
  , class Prime
  , inverse
  ) where

import Prelude
import Data.Array as Array
import Data.Typelevel.Num (class Pos, type (:*), D1, D2, D3, D5, D7, toInt)
import Data.Typelevel.Undefined (undefined)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Gen (elements)

-- | Integers modulo some number m.
newtype Z m = Z Int

derive newtype instance eqZ :: Eq (Z m)
derive newtype instance ordZ :: Ord (Z m)
derive newtype instance showZ :: Show (Z m)

mkZ :: forall m. (Pos m) => Int -> Z m
mkZ x =
  let
    m = toInt (undefined :: m)
  in
    -- This ensures that we get a number between 0 and m-1
    Z (((x `mod` m) + m) `mod` m)

runZ :: forall m. Z m -> Int
runZ (Z x) = x

class (Pos m) <= Prime m

-- TODO: More primes
instance prime2 :: Prime D2
instance prime3 :: Prime D3
instance prime5 :: Prime D5
instance prime7 :: Prime D7
instance prime11 :: Prime (D1 :* D1)

instance semiringZ :: (Pos m) => Semiring (Z m) where
  add (Z x) (Z y) = mkZ (add x y)
  mul (Z x) (Z y) = mkZ (mul x y)
  zero = Z 0
  one = Z 1

instance ringZ :: (Pos m) => Ring (Z m) where
  sub (Z x) (Z y) = mkZ (sub x y)

instance commutativeRingZ :: (Pos m) => CommutativeRing (Z m)

instance euclideanRingZ :: (Prime m) => EuclideanRing (Z m) where
  degree _ = 1
  div x y = x * inverse y
  mod _ _ = Z 0

instance fieldZ :: (Prime m) => Field (Z m)

-- | Compute a multiplicative inverse of some number in Z_m. Note that an
-- | inverse is only guaranteed to exist if m is prime (which is required by a
-- | constraint on this function).
-- |
-- | Adapted from https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Modular_integers
inverse :: forall m. (Prime m) => Z m -> Z m
inverse (Z a) = mkZ (go 0 n 1 a)
  where
    n = toInt (undefined :: m)

    go t _ _ 0 =
      t
    go t r newt newr =
      let
        quot = r / newr
      in
        go newt newr (t - quot * newt) (r - quot * newr)

instance arbitraryZ :: (Pos m) => Arbitrary (Z m) where
  arbitrary =
    let
      m = toInt (undefined :: m)
    in
      map mkZ (elements 0 (Array.range 1 (m - 1)))
