# Module Documentation

## Module Data.ModularArithmetic


Just a proof of concept.

#### `Modulus`

``` purescript
class Modulus a where
  modulus :: Proxy a -> Number
```


#### `IsNumber`

``` purescript
class IsNumber a where
```

A class with no members. The only 'law' is that if a type has an instance,
it should have the same runtime representation as Number.

#### `modularSemiring`

``` purescript
instance modularSemiring :: (IsNumber a, Modulus a) => Semiring a
```


#### `modularRing`

``` purescript
instance modularRing :: (IsNumber a, Modulus a) => Ring a
```