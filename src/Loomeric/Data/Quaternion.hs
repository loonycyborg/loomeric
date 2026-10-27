{-# LANGUAGE TypeFamilies, DataKinds, OverloadedRecordDot #-}
module Loomeric.Data.Quaternion where

import Prelude (($), (.), Show(..), Eq(..), Foldable(..))

import Generics.SOP
import Loomeric.Data.Vector
import Loomeric.Algebra.Group
import Loomeric.Algebra.Ring
import Loomeric.Algebra.Module
import Loomeric.Algebra.Field
import Loomeric.Algebra.VectorSpace
import Data.MonoTraversable

data Quaternion a where
    Quaternion :: Field a => { s :: a, v :: Vector3D a } -> Quaternion a

deriving instance Show a => Show (Quaternion a)
deriving instance Eq a => Eq (Quaternion a)
deriving instance Foldable Quaternion
type instance Element (Quaternion a) = a
instance MonoFoldable (Quaternion a)
instance MonoFunctor (Quaternion a) where
    omap f (Quaternion s v) = Quaternion (f s) (omap f v)

instance AdditiveSemigroup (Quaternion a) where
    Quaternion s1 v1 + Quaternion s2 v2 = Quaternion (s1 + s2) (v1 + v2)

instance Field a => AdditiveMonoid (Quaternion a) where
    zero = Quaternion zero zero

instance Field a => AdditiveGroup (Quaternion a) where
    Quaternion s1 v1 - Quaternion s2 v2 = Quaternion (s1 - s2) (v1 - v2)
    negate (Quaternion s v) = Quaternion (negate s) (negate v)

instance Field a => SemiModule a (Quaternion a)
instance Field a => Module a (Quaternion a)
instance ExponentialField a => Normed a (Quaternion a) where
    normSquared x = ofoldl1Ex' (+) $ omap pow2 x where
        pow2 x = x * x
    norm = sqrt . normSquared

instance (ExponentialField a, Eq a) => MultiplicativeSemigroup (Quaternion a) where
    Quaternion s1 v1 * Quaternion s2 v2 = Quaternion (s1 * s2 - v1 `dot` v2) (s1 *^ v2 + s2 *^ v1 + v1 `cross` v2)

instance (ExponentialField a, Eq a) => MultiplicativeMonoid (Quaternion a) where
    one = Quaternion one zero

instance (ExponentialField a, Eq a) => MultiplicativeGroup (Quaternion a) where
    inverse x@(Quaternion s v) = Quaternion (s / normSquared x) (negate v ^/ normSquared x)
    x / y = x * inverse y

instance (ExponentialField a, Eq a) => Semiring (Quaternion a) where
    fromNatural n = Quaternion (fromNatural n) zero
instance (ExponentialField a, Eq a) => Ring (Quaternion a) where
    fromInteger n = Quaternion (fromInteger n) zero
instance (ExponentialField a, Eq a) => Field (Quaternion a) where
    fromRational n = Quaternion (fromRational n) zero

instance (ExponentialField a, Eq a) => InvolutionRing (Quaternion a) where
    conjugate (Quaternion s v) = Quaternion s (negate v)

instance Field a => VectorSpace (Quaternion a) a [ "s", "x", "y", "z" ]

instance Field a => HasCoordinate (Quaternion a) a "s" where
    basisVec = Quaternion one zero
    coord = (.s)

instance Field a => HasCoordinate (Quaternion a) a "x" where
    basisVec = Quaternion zero (Vector3D one zero zero)
    coord = (.x) . (.v)

instance Field a => HasCoordinate (Quaternion a) a "y" where
    basisVec = Quaternion zero (Vector3D zero one zero)
    coord = (.y) . (.v)

instance Field a => HasCoordinate (Quaternion a) a "z" where
    basisVec = Quaternion zero (Vector3D zero zero one)
    coord = (.z) . (.v)

instance (ExponentialField a, Eq a) => NormedVectorSpace (Quaternion a) a [ "s", "x", "y", "z" ]

instance (ExponentialField a, TrigonometricField a, Eq a) => ExponentialSemiring (Quaternion a) where
    x ** y = exp $ y * log x

instance (ExponentialField a, TrigonometricField a, Eq a) => ExponentialField (Quaternion a) where
    exp x = exp x.s *^ Quaternion (cos (norm x.v)) (normalizeVec x.v ^* sin (norm x.v))
    log x = Quaternion (log x.s) (if norm x == zero then zero else normalizeVec x.v ^* acos (x.s / norm x))
    sqrt x = x ** (one / (one + one))
