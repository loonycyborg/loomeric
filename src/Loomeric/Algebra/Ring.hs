{-# LANGUAGE MagicHash, UnliftedDatatypes, UnboxedTuples, TypeFamilies, DefaultSignatures #-}
module Loomeric.Algebra.Ring where

import Data.Bool
import Data.Kind
import GHC.Num.Natural
import GHC.Num.Integer
import GHC.Natural ( naturalToInteger )
import GHC.Float ( integerToFloat#, naturalToFloat# )
import GHC.Exts
import GHC.Num.Primitives (absI#, sgnI#)
import Prelude (($), (.), (==), (^), id, uncurry, concatMap, Foldable (), Ord(..))

import Loomeric.Algebra.Group
import Loomeric.Data.Conversions.Integer


{- | === An algebraic semiring

    An algebraic structure that is both monoid under addition and monoid under multiplication.
    It also should satisfy:

    * @zero * a = zero@ (Left-annihilation)
    * @a * zero = zero@ (Right-annihilation)
    * @a * (b + c) = a * c + b * c@ (Left distributivity)
    * @(b + c) * a = b * a + c * a@ (Right distributivity)

    Each semiring will have an unique homomorphism from natural numbers which is represented by
    'fromNatural' method
-}
class (AdditiveMonoid a, MultiplicativeMonoid a) => Semiring a where
    linearCombination :: Foldable f => f (a, a) -> a
    linearCombination = sum . concatMap ((:[]) . uncurry (*))
    fromNatural :: Natural -> a

{- | === An algebraic ring

    A 'Semiring' that is also a group under addition. For each ring there is an unique homomorphism
    from integer which is represented by 'fromInteger' method.
-}
class (AdditiveGroup a, Semiring a) => Ring a where
    fromInteger :: Integer -> a

class (Semiring a, Ord a) => OrderedSemiring a where
    type Signing a :: Type
    toSigned :: a -> Signing a
    default toSigned :: Signing a ~ a => a -> Signing a
    toSigned = id
    signum :: a -> a
    abs :: a -> a
    abs x = signum x * x

class (OrderedSemiring a, Ring a, Signing (Unsigning a) ~ a, OrderedSemiring (Unsigning a)) => OrderedRing a where
    type Unsigning a :: Type
    toUnsignedUnsafe :: a -> Unsigning a
    default toUnsignedUnsafe :: Unsigning a ~ a => a -> Unsigning a
    toUnsignedUnsafe = id
    absG :: (b ~ Unsigning a) => a -> b
    absG = toUnsignedUnsafe . abs

type Num a = OrderedRing a

class Semiring a => InvolutionRing a where
    conjugate :: a -> a

class Semiring a => ExponentialSemiring a where
    (**) :: a -> a -> a

infixr 8 **

instance Semiring Word where
    fromNatural = naturalToWord

instance OrderedSemiring Word where
    type Signing Word = Int
    toSigned (W# x) = I# $ word2Int# x
    signum x = bool 1 0 $ x == 0
    abs = id

instance ExponentialSemiring Word where
    (W# x) ** (W# y) = W# $ binpow x y where
        binpow :: Word# -> Word# -> Word#
        binpow x# y# = case (# y#, y# `and#` 1## #) of
            (# 0##, _   #) -> 1##
            (# _  , 1## #) -> timesWord# (shifted_pow x# y#) x#
            (# _  , _   #) -> shifted_pow x# y#
            where
            pow2 x = timesWord# x x
            shifted_pow x y = pow2 $ binpow x (shiftRL# y 1#)

instance Semiring Int where
    fromNatural = integerToInt . naturalToInteger
instance Ring Int where
    fromInteger = integerToInt
instance OrderedSemiring Int where
    type Signing Int = Int
    abs (I# x) = I# $ absI# x
    signum (I# x) = I# $ sgnI# x
instance OrderedRing Int where
    type Unsigning Int = Word
    toUnsignedUnsafe (I# x) = W# $ int2Word# x

instance Semiring Float where
    fromNatural x = F# $ naturalToFloat# x
instance Ring Float where
    fromInteger x = F# $ integerToFloat# x
instance OrderedSemiring Float where
    type Signing Float = Float
    signum x
        | x > 0     = 1
        | x < 0     = -1
        | otherwise = x
    abs (F# x) = F# $ fabsFloat# x
instance OrderedRing Float where
    type Unsigning Float = Float

instance ExponentialSemiring Float where
    (F# a) ** (F# b) = F# $ powerFloat# a b

instance Semiring Natural where
    fromNatural = id
instance OrderedSemiring Natural where
    type Signing Natural = Integer
    toSigned = naturalToInteger
    signum = naturalSignum
    abs = id
instance ExponentialSemiring Natural where
    (**) = (^)

instance Semiring Integer where
    fromNatural = naturalToInteger
instance Ring Integer where
    fromInteger = id

instance OrderedSemiring Integer where
    type Signing Integer = Integer
    abs = integerAbs
    signum = integerSignum

instance OrderedRing Integer where
    type Unsigning Integer = Natural
    toUnsignedUnsafe = integerToNatural
