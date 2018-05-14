-- This module investigates Wilson's Theorem, which says that
--
--   (n - 1)! is congruent to -1 mod n if and only if n is prime.
--
module Example.Main where

import Prelude
import Control.Monad.Eff.Console (log)
import Data.Foldable (foldl, for_)
import Data.Array as Array
import Data.ModularArithmetic (Z, mkZ, runZ, isPrime)
import Data.Typelevel.Num.Sets (class Pos, reifyIntP)

-- | Compute a factorial modulo m.
modFact :: forall m. Pos m => Int -> Z m
modFact n = foldl mul' one (Array.range 1 n)
  where
  -- We perform all multiplications modulo m to reduce the risk of overflow.
  mul' :: Z m -> Int -> Z m
  mul' x y = x * mkZ y

checkWilson :: Int -> { n :: Int, result :: Int, prime :: Boolean }
checkWilson n =
  let
    go :: forall m. Pos m => m -> Int
    go _ = runZ (modFact (n-1) :: Z m)

    result = reifyIntP n go
  in
    { n, result, prime: isPrime n }

main =
  for_ (Array.range 2 50) \n -> do
    let {result, prime} = checkWilson n
    let isOrIsNot = if prime then "is" else "is not"
    log $
      "n=" <> show n <> ":\t\t" <>
        "(n-1)! ≡ " <> show result <> " mod n\t\t" <>
        "n " <> isOrIsNot <> " prime"
