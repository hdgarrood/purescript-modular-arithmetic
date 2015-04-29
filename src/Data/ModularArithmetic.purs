-- | Just a proof of concept.

module Data.ModularArithmetic
  ( Modulus
  , modulus
  , IsNumber
  ) where

import Data.Function
import Type.Proxy
import Debug.Trace

class Modulus a where
  modulus :: Proxy a -> Number

-- | A class with no members. The only 'law' is that if a type has an instance,
-- | it should have the same runtime representation as Number.
class IsNumber a

foreign import coerceNumber
  """
  function coerceNumber(dict_ignored) {
    return function(x) {
      return x
    }
  }
  """ :: forall a. (IsNumber a) => Number -> a

foreign import addImpl
  """
  function addImpl(dict_ignored) {
    return function(modulus, x, y) {
      var z = x + y;
      return ((z % modulus) + modulus) % modulus;
    }
  }
  """ :: forall a. (IsNumber a) => Fn3 Number a a a

add :: forall a. (IsNumber a) => Number -> a -> a -> a
add m x y = runFn3 addImpl m x y

foreign import mulImpl
  """
  function mulImpl(dict_ignored) {
    return function(modulus, x, y) {
      var z = x * y;
      return ((z % modulus) + modulus) % modulus;
    }
  }
  """ :: forall a. (IsNumber a) => Fn3 Number a a a

mul :: forall a. (IsNumber a) => Number -> a -> a -> a
mul m x y = runFn3 mulImpl m x y

foreign import subImpl
  """
  function subImpl(dict_ignored) {
    return function(modulus, x, y) {
      var z = x - y;
      return ((z % modulus) + modulus) % modulus;
    }
  }
  """ :: forall a. (IsNumber a) => Fn3 Number a a a

sub :: forall a. (IsNumber a) => Number -> a -> a -> a
sub m x y = runFn3 subImpl m x y

instance modularSemiring :: (IsNumber a, Modulus a) => Semiring a where
  (+) = add (modulus (Proxy :: Proxy a))
  (*) = mul (modulus (Proxy :: Proxy a))
  zero = coerceNumber 0
  one = coerceNumber 1

instance modularRing :: (IsNumber a, Modulus a) => Ring a where
  (-) = sub (modulus (Proxy :: Proxy a))
