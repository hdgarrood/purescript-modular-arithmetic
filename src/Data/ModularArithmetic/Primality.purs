module Data.ModularArithmetic.Primality
  ( primeFactors
  , isPrime
  , class Prime
  , reifyPrime
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.List (List(Nil), (:))
import Data.List as List
import Data.Typelevel.Num (class Pos, type (:*), D1, D2, D3, D4, D5, D6, D7, D8, D9, reifyIntP)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- | Find prime factors by _trial division_; first attempting to divide by 2
-- | and then by every odd number after that. This is the most basic prime
-- | factorisation algorithm possible but it is more than enough for this case
-- | (specifically, when the input is guaranteed to be no more than 2^31).
-- |
-- | Prime factors are returned in increasing order. For example:
-- |
-- | ```purescript
-- | > trialDivision 12
-- | (2 : 2 : 3 : Nil)
-- | ```
-- |
-- | Passing in any number less than 2 will return an empty list.
-- |
-- | ```purescript
-- | > trialDivision (-12)
-- | Nil
-- | ```
-- |
-- | For all positive integers, the following properties are satisfied:
-- |
-- | ```purescript
-- | > product (primeFactors n) == n
-- | > all isPrime (primeFactors n)
-- | ```
primeFactors :: Int -> List Int
primeFactors = go Nil 2 <<< sanitize
  where
  go factors _ 1 =
    List.reverse factors
  go factors divisor n =
    if n `mod` divisor == 0
      then go (divisor : factors) divisor (n / divisor)
      else go factors (nextDivisor divisor) n

  nextDivisor 2 = 3
  nextDivisor k = k + 2

  sanitize n = if n < 2 then 1 else n

-- | Check if a number is prime. Note that 1 is not a prime number.
isPrime :: Int -> Boolean
isPrime n = primeFactors n == List.singleton n

-- | This class specifies that a type-level integer is *prime*; that is, it
-- | has exactly 2 divisors: itself, and 1.
-- |
-- | All primes up to 100 have instances. For larger primes, you will need to
-- | use the `reifyPrime` function.
class Pos m <= Prime m

instance prime2 :: Prime D2
instance prime3 :: Prime D3
instance prime5 :: Prime D5
instance prime7 :: Prime D7
instance prime11 :: Prime (D1 :* D1)
instance prime13 :: Prime (D1 :* D3)
instance prime17 :: Prime (D1 :* D7)
instance prime19 :: Prime (D1 :* D9)
instance prime23 :: Prime (D2 :* D3)
instance prime29 :: Prime (D2 :* D9)
instance prime31 :: Prime (D3 :* D1)
instance prime37 :: Prime (D3 :* D7)
instance prime41 :: Prime (D4 :* D1)
instance prime43 :: Prime (D4 :* D3)
instance prime47 :: Prime (D4 :* D7)
instance prime53 :: Prime (D5 :* D3)
instance prime59 :: Prime (D5 :* D9)
instance prime61 :: Prime (D6 :* D1)
instance prime67 :: Prime (D6 :* D7)
instance prime71 :: Prime (D7 :* D1)
instance prime73 :: Prime (D7 :* D3)
instance prime79 :: Prime (D7 :* D9)
instance prime83 :: Prime (D8 :* D3)
instance prime89 :: Prime (D8 :* D9)
instance prime97 :: Prime (D9 :* D7)

-- | Reify a prime number at the type level. If the first argument provided is
-- | not prime, this function returns `Nothing`.
reifyPrime :: forall r. Int -> (forall p. Prime p => p -> r) -> Maybe r
reifyPrime n f =
  if isPrime n
    then Just (reifyIntP n (relax f))
    else Nothing

relax :: forall r.
  (forall p. Prime p => p -> r) ->
  (forall p. Pos p   => p -> r)
relax = unsafeCoerce relax'

-- HACK: this relies on the runtime representation of superclasses.
type DictPrime dictPos = { "Pos0" :: Unit -> dictPos }

relax' :: forall r dictPos. (DictPrime dictPos -> r) -> (dictPos -> r)
relax' f dictPos =
  f (upcastDictPos dictPos)

upcastDictPos :: forall r dictPos.  dictPos -> DictPrime dictPos
upcastDictPos dictPos = { "Pos0": const dictPos }
