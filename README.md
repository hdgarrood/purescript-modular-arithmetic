# purescript-modular-arithmetic

This library provides a `newtype` over `Int` which can represent the ring of
integers modulo n for any positive integer n, sometimes written ℤ/nℤ.

If you haven't heard of this before, we will use the integers modulo 12 as an
example. There are 12 distinct elements of this set: 0, 1, 2, ... 9, 10, 11.
(In general, there are n elements; 0 up to n-1).  Addition works as normal,
except that if the result is larger than 12, we loop around to 0 again so that
we stay in the same set. For example, with integers modulo 12, 2 + 3 = 5 (as
normal), but 10 + 5 = 3. Multiplication works in a similar way, looping back
around if the result would be too large so for example 3 * 6 = 6.

What's the point of this library though? Well, we do often want to deal with
integers modulo n in 'real code'. For instance, we might use integers modulo n
to represent player IDs in a turn-based game of n players: then, the function
for selecting the next player to take a turn is simply `(_ + one)`; this will
then automatically loop back to 0 after all the players have taken a turn.

Another application of this library is for testing libraries which abstract
over the numeric type classes defined in the Prelude. `Int` and `Number` are
often not suitable for testing with, because they do not always abide by the
relevant type class laws. By comparison, the types provided by this library are
always *fully law-abiding* (well, as long as you don't ask for integers modulo
some number larger than 2^31).

In fact, you even get a (fully law-abiding) `Field` when n is a prime number!
We can divide elements of ℤ/nℤ when n is prime by finding the multiplicative
inverse of the divisor: that is, the number that you have to multiply it by to
get 1. Then, x divided by y is the same as x times the inverse of y. The prime
number restriction is required to ensure that every number has a unique
inverse. So for example, in ℤ/7ℤ, the multiplicative inverse of 5 is 3, because
5 * 3 = 1. Therefore, 4 / 5 = 4 * 3 = 5. This still behaves how we would hope,
since multiplying by 5 again gets us back to where we started: 5 * 5 = 4.

Documentation is [on Pursuit](https://pursuit.purescript.org/packages/purescript-modular-arithmetic).
