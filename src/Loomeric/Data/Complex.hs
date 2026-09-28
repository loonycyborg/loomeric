{-# LANGUAGE TypeFamilies, DataKinds, OverloadedRecordDot #-}
module Loomeric.Data.Complex where

import Data.MonoTraversable
import GHC.Records
import GHC.TypeLits

import Loomeric.Algebra.Group
import Loomeric.Algebra.Ring
import Loomeric.Algebra.Field
import Loomeric.Algebra.Module
import Loomeric.Algebra.VectorSpace

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

instance Field a => MultiplicativeGroup (Complex a) where
    (x :+ x') / (y :+ y') = linearCombination [ (x, y), (x', y') ] / d :+ linearCombination [ (x', y), (negate x, y') ] / d where
        d = linearCombination [ (y, y), (y', y') ]
    inverse a@(x :+ x') = omap (/linearCombination [ (x, x) , (x', x') ]) $ conjugate a

instance Ring a => Semiring (Complex a) where
    fromNatural n = fromNatural n :+ zero

instance Ring a => Ring (Complex a) where
    fromInteger n = fromInteger n :+ zero

instance Ring a => InvolutionRing (Complex a) where
    conjugate (x :+ x') = x :+ negate x'

instance ExponentialField a => Field (Complex a) where
    fromRational n = fromRational n :+ zero

instance (ExponentialField a, Eq a, Ord a, TrigonometricField a) => ExponentialField (Complex a) where
    sqrt a@(x :+ x') = s1 * sqrt ((a.r + x)/(one + one)) :+ s2 * sqrt ((a.r - x)/(one + one)) where
        s1 = one
        s2 = if x' >= zero then one else negate one
    exp a@(x :+ x') = exp x *^ (cos x' :+ sin x')
    log x = log x.r :+ x.theta
    x ** y = exp $ y * log x

instance Semiring a => SemiModule a (Complex a)
instance Ring a => Module a (Complex a)
instance ExponentialField a => Normed a (Complex a) where
    norm (x :+ x') = sqrt $ x * x + x' * x'

instance Ring a => VectorSpace [ "real", "imag" ] a (Complex a)

instance Ring a => HasCoordinate "real" a (Complex a) where
    basisVec = one :+ zero
    coord = realPart

instance Ring a => HasCoordinate "imag" a (Complex a) where
    basisVec = zero :+ one
    coord = imagPart

instance (Ring a, HasCoordinate s a (Complex a)) => HasField (s :: Symbol) (Complex a) a where
    getField = coord @s

instance {-# OVERLAPPING #-} ExponentialField a => HasField "r" (Complex a) a where
    getField = norm

instance {-# OVERLAPPING #-} (Eq a, TrigonometricField a) => HasField "theta" (Complex a) a where
    getField = phase

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
