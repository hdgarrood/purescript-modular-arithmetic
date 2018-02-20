module Data.ModularArithmetic
  ( Z
  , mkZ
  , runZ
  , class Prime
  , inverse
  , enumerate
  , genZ
  ) where

import Prelude

import Control.Monad.Gen (class MonadGen, chooseInt)
import Data.Array as Array
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, (:|))
import Data.Typelevel.Num (class Pos, type (:*), D1, D2, D3, D5, D7, toInt)
import Data.Typelevel.Undefined (undefined)

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

-- | This class specifies that a type-level integer is *prime*; that is, it
-- | has exactly 2 divisors: itself, and 1.
-- |
-- | Sadly, only a small number of primes have instances right now. Hopefully
-- | this will change in the future.
class Pos m <= Prime m

-- TODO: More primes
instance prime2 :: Prime D2
instance prime3 :: Prime D3
instance prime5 :: Prime D5
instance prime7 :: Prime D7
instance prime11 :: Prime (D1 :* D1)

instance semiringZ :: Pos m => Semiring (Z m) where
  add (Z x) (Z y) = mkZ (add x y)
  mul (Z x) (Z y) = mkZ (mul x y)
  zero = Z 0
  one = mkZ 1

instance ringZ :: Pos m => Ring (Z m) where
  sub (Z x) (Z y) = mkZ (sub x y)

instance commutativeRingZ :: Pos m => CommutativeRing (Z m)

instance euclideanRingZ :: Prime m => EuclideanRing (Z m) where
  degree _ = 1
  div x y = x * inverse y
  mod _ _ = Z 0

instance fieldZ :: Prime m => Field (Z m)

-- | Compute a multiplicative inverse of some number in Z_m. Note that an
-- | inverse is only guaranteed to exist if m is prime (which is required by a
-- | constraint on this function).
-- Adapted from https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Modular_integers
inverse :: forall m. Prime m => Z m -> Z m
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

-- | List all members of Z_m.
enumerate :: forall m. Pos m => NonEmpty Array (Z m)
enumerate =
  let
    m = toInt (undefined :: m)
  in
    mkZ 0 :| (if m == 1 then [] else map mkZ (Array.range 1 (m - 1)))

-- | Create a random generator for members of Z_m.
genZ :: forall m n. MonadGen m => Pos n => m (Z n)
genZ = Z <$> chooseInt 0 (toInt (undefined :: n) - 1)
