{-# LANGUAGE TypeFamilies #-}
module Loomeric.Complex where

import Data.MonoTraversable

import Loomeric.Group
import Loomeric.Ring
import Loomeric.Field
import Loomeric.Module

import Prelude (($), (.), otherwise, Eq(..), Ord(..), Show(..), Read(..), Foldable(..))

data Complex a where
    (:+) :: Semiring a => a -> a -> Complex a
infix 6 :+

type instance Element (Complex a) = a

deriving instance Show a => Show (Complex a)
deriving instance (Read a, Semiring a) => Read (Complex a)
deriving instance Eq a => Eq (Complex a)
deriving instance Foldable Complex
instance MonoFoldable (Complex a)

instance MonoFunctor (Complex a) where
    omap f (x :+ x') = f x :+ f x'

instance AdditiveSemigroup (Complex a) where
    (x :+ x') + (y :+ y') = (x + y) :+ (x' + y')

instance Semiring a => AdditiveMonoid (Complex a) where
    zero = zero :+ zero

instance Ring a => AdditiveGroup (Complex a) where
    (x :+ x') - (y :+ y') = (x - y) :+ (x' - y')
    negate = omap negate

instance Ring a => MultiplicativeSemigroup (Complex a) where
    (x :+ x') * (y :+ y') = linearCombination [ (x, y), (negate x', y') ] :+ linearCombination [ (x', y), (x, y') ]

instance Ring a => MultiplicativeMonoid (Complex a) where
    one = one :+ zero

instance ExponentialField a => MultiplicativeGroup (Complex a) where
    (x :+ x') / b@(y :+ y') = linearCombination [ (x, y), (x', y') ] / d :+ linearCombination [ (x', y), (negate x, y') ] / d where
        d = linearCombination [ (y, y), (y', y') ]
    inverse x = conjugate x ^* (inverse $ norm x :: a)

instance Ring a => Semiring (Complex a) where
    fromNatural n = fromNatural n :+ zero

instance Ring a => Ring (Complex a) where
    fromInteger n = fromInteger n :+ zero

instance ExponentialField a => Field (Complex a) where
    fromRational n = fromRational n :+ zero

instance (ExponentialField a, Eq a, Ord a, TrigonometricField a) => ExponentialField (Complex a) where
    sqrt a@(x :+ x') = s1 * sqrt ((norm a + x)/(one + one)) :+ s2 * sqrt ((norm a - x)/(one + one)) where
        s1 = one
        s2 = if x' >= zero then one else negate one
    exp a@(x :+ x') = exp x *^ (cos x' :+ sin x')
    log x = log (norm x) :+ phase x
    x ** y = exp $ y * log x

instance Semiring a => SemiModule a (Complex a)
instance Ring a => Module a (Complex a)
instance ExponentialField a => Normed a (Complex a) where
    norm (x :+ x') = sqrt $ x * x + x' * x'

realPart :: Complex a -> a
realPart (x :+ _) = x

imagPart :: Complex a -> a
imagPart (_ :+ x') = x'

magnitude :: ExponentialField a => Complex a -> a
magnitude = norm

phase :: (Eq a, TrigonometricField a) => Complex a -> a
phase a@(x :+ x')
        | a == zero = zero
        | otherwise = atan2 x' x

conjugate :: AdditiveGroup a => Complex a -> Complex a
conjugate (x :+ x') = x :+ negate x'
