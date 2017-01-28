module Data.ModularArithmetic
  ( Z
  , mkZ
  , runZ
  , class Prime
  , inverse
  ) where

import Prelude
import Type.Proxy
import Data.Typelevel.Num (class Mul, class Nat, class Pos, class Succ, type (:*), D0, D1, D11, D2, D3, D5, D7, toInt)
import Data.Typelevel.Undefined (undefined)

-- | Integers modulo some number m.
newtype Z m = Z Int

derive newtype instance showZ :: Show (Z m)

mkZ :: forall m. (Pos m) => Int -> Z m
mkZ x = Z (x `mod` toInt (undefined :: m))

runZ :: forall m. Z m -> Int
runZ (Z x) = x

-- | Exponentation of natural numbers at the type level.
class (Nat x, Nat y, Nat z) <= Exp x y z

-- | exp(x, 0) = 1
instance expZero :: Nat x => Exp x D0 D1
-- | exp(x, y) = y * exp(x, y-1)
instance expPos :: (Nat x, Nat o, Pos y, Succ y y1, Exp x y1 z, Mul y z o) => Exp x y o

class (Pos m) <= Prime m

-- TODO: More primes
instance prime2 :: Prime D2
instance prime3 :: Prime D3
instance prime5 :: Prime D5
instance prime7 :: Prime D7
instance prime11 :: Prime (D1 :* D1)

-- A number of the form p^k, for some prime p and positive integer k.
class PrimePower m

instance primePower :: (Prime p, Pos k, Exp p k q) => PrimePower q

instance semiringZ :: (Pos m) => Semiring (Z m) where
  add (Z x) (Z y) = mkZ (add x y)
  mul (Z x) (Z y) = mkZ (mul x y)
  zero = Z 0
  one = Z 1

instance ringZ :: (Pos m) => Ring (Z m) where
  sub (Z x) (Z y) = mkZ (sub x y)

instance commutativeRingZ :: (Pos m) => CommutativeRing (Z m)

instance euclideanRingZ :: (Prime m) => EuclideanRing (Z m) where
  degree = runZ
  div x y = x * inverse y
  mod x y = x - (div x y)

-- | Compute a multiplicative inverse of some number in Z_m. Note that an
-- | inverse is only guaranteed to exist if m is prime (which is required by a
-- | constraint on this function).
-- |
-- | Adapted from https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Modular_integers
inverse :: forall m. (Prime m) => Z m -> Z m
inverse (Z a) = Z (go 0 n 1 a)
  where
    n = toInt (undefined :: m)

    go t _ _ 0 =
      if t < 0 then t + n else t
    go t r newt newr =
      let
        quot = r / newr
      in
        go newt (t - quot * newt) newr (r - quot * newr)
