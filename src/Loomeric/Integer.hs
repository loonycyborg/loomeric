{-# LANGUAGE MagicHash, UnboxedTuples, DefaultSignatures #-}
module Loomeric.Integer where

import Data.Maybe
import Control.Applicative
import Control.Category
import Control.Monad
import GHC.Num.Natural
import GHC.Num.Integer
import GHC.Num.BigNat
import GHC.Exts

import Prelude (fst, snd, not, (==), ($), otherwise, error, Ord(..))

import Loomeric.Group
import Loomeric.Ring

infixl 7 `quot`, `rem`, /?
infixl 6 -?

{- | === A partial group under addition

    An additive monoid that has subtraction operation that it is only actually defined for
    /some/ pairs of elements. Partial subtraction '-?' returns 'Nothing' for elements which can't be
    subtracted. It should satisfy:

    * @a -? a = Just zero@
    * If @a -? b = Just c@ then @a = c + b@
-}
class AdditiveMonoid a => AdditivePartialGroup a where
    (-?) :: a -> a -> Maybe a
    default (-?) :: PeanoSystem a => a -> a -> Maybe a
    a -? b = case decrement b of
        Nothing -> Just a
        Just p  -> (-? p) =<< decrement a
    -- | Calls 'error' for elements that can't be subtracted instead
    (-!) :: a -> a -> a
    a -! b = fromJust $ (a -? b) <|> error "Subtraction underflow"

{- | === A partial group under multiplication

    A multiplicative monoid that has partial division operation '/?' that satisfies:

    * @a /? a = Just one@
    * if @a /? b = Just c@ then @a = c * b@

    '/?' returns 'Nothing' for elements for which division isn't defined
-}
class MultiplicativeMonoid a => MultiplicativePartialGroup a where
    (/?) :: a -> a -> Maybe a
    default (/?) :: EuclideanDomain a => a -> a -> Maybe a
    a /? b = case quotRem a b of
        (result, z) | z == zero -> Just result
        _                       -> Nothing


{- | === A Peano-like set

    A set with zero element and operation that takes an element and returns the next one.
    Only types where /all/ elements can be built from 'zero' by chain of increments
    should implement this typeclass.
-}
class (Semiring a, AdditivePartialGroup a) => PeanoSystem a where
    increment :: a -> a
    decrement :: a -> Maybe a

{- | === Euclidean Domain

    An ordered 'Semiring' or 'Ring' of integers or similar objects that have partial
    division defined as well as GCD and LCM. 'Fields' and other structures that are
    closed under division (except by zero) should not define this typeclass.
-}
class (OrderedSemiring a, MultiplicativePartialGroup a) => EuclideanDomain a where
    quotRem :: a -> a -> (a,a)
    quot :: a -> a -> a
    quot = (fst <$>) . quotRem
    rem :: a -> a -> a
    rem = (snd <$>) . quotRem
    gcd :: a -> a -> a
    gcd x y = gcdAbs (abs x) (abs y) where
        gcdAbs a b | b == zero = a
                   | otherwise = gcdAbs b (a `rem` b)
    lcm :: a -> a -> a
    lcm x y | y == zero = zero
            | x == zero = zero
            | otherwise = abs ((x `quot` gcd x y) * y)

type Integral a = EuclideanDomain a

instance AdditivePartialGroup Natural where
    x -? y = case naturalSub x y of
        (# (# #) | #) -> Nothing
        (# | res #)   -> Just res

instance AdditivePartialGroup Word where
    (W# a) -? (W# b) = case subWordC# a b of
        (# result, 0# #) -> Just $ W# result
        (# _     , _  #) -> Nothing

instance MultiplicativePartialGroup Natural

instance MultiplicativePartialGroup Integer

instance MultiplicativePartialGroup Int

instance MultiplicativePartialGroup Word

instance PeanoSystem Word where
    increment (W# x) = W# $ plusWord# x 1##
    decrement (W# x) = if isTrue# $ x `eqWord#` 0## then
        Nothing
            else
        Just $ W# $ minusWord# x 1##

instance EuclideanDomain Word where
    quotRem (W# x) (W# y) = case quotRemWord# x y of
        (# r, c #) -> (W# r, W# c)
    gcd (W# x) (W# y) = W# $ gcdWord# x y

instance EuclideanDomain Int where
    quotRem (I# a) (I# b) = case quotRemInt# a b of
        (# x, y #) -> (I# x, I# y)
    gcd (I# x) (I# y) = I# $ gcdInt# x y

instance PeanoSystem Natural where
    increment x = naturalAdd x one
    decrement x = x -? one

instance EuclideanDomain Natural where
    quotRem = naturalQuotRem
    gcd = naturalGcd
    lcm = naturalLcm

instance EuclideanDomain Integer where
    quotRem = integerQuotRem
    gcd = integerGcd
    lcm = integerLcm

even :: EuclideanDomain a => a -> Bool
even x = x `rem` (one + one) == zero

odd :: EuclideanDomain a => a -> Bool
odd = not . even
