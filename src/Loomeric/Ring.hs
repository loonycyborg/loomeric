{-# LANGUAGE MagicHash #-}
module Loomeric.Ring where

import Data.Bool
import GHC.Num.Natural
import GHC.Num.Integer
import GHC.Natural ( naturalToInteger )
import GHC.Float ( integerToFloat#, naturalToFloat# )
import GHC.Exts
import GHC.Num.Primitives (absI#, sgnI#)
import Prelude (($), (.), (==), id, uncurry, concatMap, Foldable (), Ord(..))

import Loomeric.Group
import Loomeric.Conversions

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
    signum :: a -> a
    abs :: a -> a
    abs x = signum x * x
    absG :: (SignedType b ~ a, SignTruncate b) => a -> b
    absG = signTruncate . abs

class (OrderedSemiring a, Ring a) => OrderedRing a

type Num a = OrderedRing a

instance Semiring Word where
    fromNatural = naturalToWord

instance OrderedSemiring Word where
    signum x = bool 1 0 $ x == 0
    abs = id

instance Semiring Int where
    fromNatural = integerToInt . naturalToInteger
instance Ring Int where
    fromInteger = integerToInt
instance OrderedSemiring Int where
    abs (I# x) = I# $ absI# x
    signum (I# x) = I# $ sgnI# x
instance OrderedRing Int

instance Semiring Float where
    fromNatural x = F# $ naturalToFloat# x
instance Ring Float where
    fromInteger x = F# $ integerToFloat# x
instance OrderedSemiring Float where
    signum x
        | x > 0     = 1
        | x < 0     = -1
        | otherwise = x
    abs (F# x) = F# $ fabsFloat# x
instance OrderedRing Float

instance Semiring Natural where
    fromNatural = id
instance OrderedSemiring Natural where
    signum = naturalSignum
    abs = id

instance Semiring Integer where
    fromNatural = naturalToInteger
instance Ring Integer where
    fromInteger = id

instance OrderedSemiring Integer where
    abs = integerAbs
    signum = integerSignum

instance OrderedRing Integer
