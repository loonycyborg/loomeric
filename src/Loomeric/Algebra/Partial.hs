{-# LANGUAGE MagicHash, UnboxedTuples, NamedDefaults #-}
module Loomeric.Algebra.Partial where

import Data.Maybe
import Control.Applicative
import GHC.Exts
import GHC.Base
import GHC.Num.Natural
import GHC.Num.Integer
import GHC.Num.BigNat

import Prelude (($), (.), (==), fst, snd, otherwise, Ord(..), error)

import Loomeric.Algebra.Group

infixl 7 /?, /!
infixl 6 -?, -!, -!!

{- | === A partial group under addition

    An additive monoid that has subtraction operation that it is only actually defined for
    /some/ pairs of elements. Partial subtraction '-?' returns 'Nothing' for elements which can't be
    subtracted. It should satisfy:

    * @a -? a = Just zero@
    * If @a -? b = Just c@ then @a = c + b@
-}
class AdditiveMonoid a => AdditivePartialGroup a where
    (-?) :: a -> a -> Maybe a
    -- | Calls 'error' for elements that can't be subtracted instead
    (-!) :: a -> a -> a
    a -! b = fromJust $ (a -? b) <|> error "Subtraction underflow"
    -- | Results in undefined behavior such as wraparound if second argument is greater.
    (-!!) :: a -> a -> a

{- | === A partial group under multiplication

    A multiplicative monoid that has partial division operation '/?' that satisfies:

    * @a /? a = Just one@
    * if @a /? b = Just c@ then @a = c * b@

    '/?' returns 'Nothing' for elements for which division isn't defined
-}
class MultiplicativeMonoid a => MultiplicativePartialGroup a where
    (/?) :: a -> a -> Maybe a
    -- | Calls 'error' for elements that can't be subtracted instead
    (/!) :: a -> a -> a
    x /! y = fromJust $ (x /? y) <|> error "Not divisible"

instance AdditivePartialGroup Natural where
    x -? y = case naturalSub x y of
        (# (# #) | #) -> Nothing
        (# | res #)   -> Just res
    x -!! y = naturalSubUnsafe x y

instance AdditivePartialGroup Word where
    (W# a) -? (W# b) = case subWordC# a b of
        (# result, 0# #) -> Just $ W# result
        (# _     , _  #) -> Nothing
    (W# x) -!! (W# y) = W# $ case subWordC# x y of (# result, _ #) -> result

instance MultiplicativePartialGroup Natural where
    x /? y = case naturalQuotRem x y of
        (result, 0) -> Just result
        _           -> Nothing

instance MultiplicativePartialGroup Integer where
    x /? y = case integerDivMod x y of
        (result, 0) -> Just result
        _           -> Nothing

instance MultiplicativePartialGroup Int where
    x /? y = case divModInt x y of
        (result, 0) -> Just result
        _           -> Nothing

instance MultiplicativePartialGroup Word where
    (W# x) /? (W# y) = case quotRemWord# x y of
        (# result, 0## #) -> Just $ W# result
        _                 -> Nothing
