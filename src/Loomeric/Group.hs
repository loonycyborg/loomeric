{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples #-}
module Loomeric.Group where

import Data.Ord
import Data.Foldable1
import Data.Foldable hiding (sum)
import GHC.Exts
import GHC.Num.Primitives (absI#, sgnI#)
import Data.List.NonEmpty as NE
import GHC.Num.Natural
import GHC.Num.Integer
import Prelude (($), fst, snd, not, uncurry, Eq(..), error)
import Control.Applicative
import Control.Monad
import Control.Category
import Data.Maybe
import Data.Bool

class AdditiveSemigroup a where
    (+) :: a -> a -> a
    sum1 :: Foldable1 f => f a -> a
    sum1 = foldl1' (+)

class AdditiveSemigroup a => AdditiveMonoid a where
    zero :: a
    sum :: Foldable f => f a -> a
    sum = foldl' (+) zero

class AdditiveMonoid a => AdditiveGroup a where
    negate :: a -> a
    (-) :: a -> a -> a

class MultiplicativeSemigroup a where
    (*) :: a -> a -> a
    product1 :: Foldable1 f => f a -> a
    product1 = foldl1' (*)

class MultiplicativeSemigroup a => MultiplicativeMonoid a where
    one :: a
    product :: Foldable f => f a -> a
    product = foldl' (*) one

class MultiplicativeMonoid a => MultiplicativeGroup a where
    inverse :: a -> a
    (/) :: a -> a -> a

type Fractional a = MultiplicativeGroup a

class (AdditiveMonoid a, MultiplicativeMonoid a) => Semiring a where
    linearCombination :: Foldable f => f (a, a) -> a
    linearCombination = sum . concatMap ((:[]) . uncurry (*))

class (AdditiveGroup a, Semiring a) => Ring a where

class (MultiplicativeGroup a, Ring a) => Field a where
    (**) :: a -> a -> a
    exp :: a -> a
    log :: a -> a

class (Semiring a, Ord a) => OrderedSemiring a where
    signum :: a -> a
    abs :: a -> a
    abs x = signum x * x

class (OrderedSemiring a, Ring a) => OrderedRing a

type Num a = OrderedRing a

class Semiring a => PeanoSystem a where
    increment :: a -> a
    decrement :: a -> Maybe a
    (-?) :: a -> a -> Maybe a
    a -? b = case decrement b of
        Nothing -> Just a
        Just p  -> (-? p) =<< decrement a
    (-!) :: a -> a -> a
    a -! b = fromJust $ (a -? b) <|> error "Subtraction underflow"

class OrderedSemiring a => EuclideanDomain a where
    quotRem :: a -> a -> (a,a)
    quot :: a -> a -> a
    quot = (fst <$>) . quotRem
    rem :: a -> a -> a
    rem = (snd <$>) . quotRem
    (/?) :: a -> a -> Maybe a
    a /? b = case quotRem a b of
        (result, z) | z == zero -> Just result
        _                       -> Nothing

type Integral a = EuclideanDomain a

instance AdditiveSemigroup Word where
    W# a + W# b = W# $ plusWord# a b

instance AdditiveMonoid Word where
    zero = 0

instance MultiplicativeSemigroup Word where
    W# a * W# b = W# $ timesWord# a b

instance MultiplicativeMonoid Word where
    one = 1

instance Semiring Word

instance OrderedSemiring Word where
    signum x = bool 1 0 $ x == 0
    abs = id

instance PeanoSystem Word where
    increment (W# x) = W# $ plusWord# x 1##
    decrement (W# x) = if isTrue# $ x `eqWord#` 0## then
        Nothing
            else
        Just $ W# $ minusWord# x 1##
    (W# a) -? (W# b) = case subWordC# a b of
        (# result, 0# #) -> Just $ W# result
        (# _     , _  #) -> Nothing

instance EuclideanDomain Word where
    quotRem (W# x) (W# y) = case quotRemWord# x y of
        (# r, c #) -> (W# r, W# c)

instance AdditiveSemigroup Int where
    I# a + I# b = I# (a +# b)

instance AdditiveMonoid Int where
    zero = 0

instance AdditiveGroup Int where
    I# a - I# b = I# (a -# b)
    negate (I# a) = I# $ negateInt# a

instance MultiplicativeSemigroup Int where
    I# a * I# b = I# (a *# b)

instance MultiplicativeMonoid Int where
    one = 1

instance Semiring Int
instance Ring Int
instance OrderedSemiring Int where
    abs (I# x) = I# $ absI# x
    signum (I# x) = I# $ sgnI# x
instance OrderedRing Int

instance EuclideanDomain Int where
    quotRem (I# a) (I# b) = case quotRemInt# a b of
        (# x, y #) -> (I# x, I# y)

instance AdditiveSemigroup Float where
    F# a + F# b = F# $ plusFloat# a b

instance AdditiveMonoid Float where
    zero = 0

instance AdditiveGroup Float where
    F# a - F# b = F# $ minusFloat# a b
    negate (F# a) = F# $ negateFloat# a

instance MultiplicativeSemigroup Float where
    F# a * F# b = F# $ timesFloat# a b

instance MultiplicativeMonoid Float where
    one = 1

instance MultiplicativeGroup Float where
    F# a / F# b = F# $ divideFloat# a b
    inverse (F# a) = F# $ divideFloat# 1.0# a

instance Semiring Float
instance Ring Float
instance OrderedSemiring Float where
    signum x
        | x > 0     = 1
        | x < 0     = -1
        | otherwise = x
    abs (F# x) = F# $ fabsFloat# x
instance OrderedRing Float
instance Field Float where
    (F# a) ** (F# b) = F# $ powerFloat# a b
    exp (F# x) = F# $ expFloat# x
    log (F# x) = F# $ logFloat# x

instance AdditiveSemigroup Natural where
    (+) = naturalAdd

instance AdditiveMonoid Natural where
    zero = naturalZero

instance MultiplicativeSemigroup Natural where
    (*) = naturalMul

instance MultiplicativeMonoid Natural where
    one = naturalOne

instance Semiring Natural
instance OrderedSemiring Natural where
    signum = naturalSignum
    abs = id

instance PeanoSystem Natural where
    increment x = naturalAdd x one
    x -? y = case naturalSub x y of
        (# (# #) | #) -> Nothing
        (# | res #)   -> Just res
    decrement x = x -? one

instance EuclideanDomain Natural where
    quotRem = naturalQuotRem

instance AdditiveSemigroup Integer where
    (+) = integerAdd

instance AdditiveMonoid Integer where
    zero = integerZero

instance AdditiveGroup Integer where
    (-) = integerSub
    negate = integerNegate

instance MultiplicativeSemigroup Integer where
    (*) = integerMul

instance MultiplicativeMonoid Integer where
    one = integerOne

instance Semiring Integer
instance Ring Integer

instance OrderedSemiring Integer where
    abs = integerAbs
    signum = integerSignum

instance OrderedRing Integer

instance EuclideanDomain Integer where
    quotRem = integerQuotRem

subtract :: AdditiveGroup a => a -> a -> a
subtract x y = y - x

even :: EuclideanDomain a => a -> Bool
even x = x `rem` (one + one) == zero

odd :: EuclideanDomain a => a -> Bool
odd = not . even
