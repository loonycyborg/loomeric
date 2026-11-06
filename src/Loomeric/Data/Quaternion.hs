{-# LANGUAGE TypeFamilies, DataKinds, OverloadedRecordDot, PatternSynonyms, ViewPatterns, GeneralisedNewtypeDeriving, DerivingStrategies #-}
module Loomeric.Data.Quaternion where

import Prelude (($), (.), Show(..), Eq(..), Foldable(..))

import Data.MonoTraversable
import Data.Vector.Unboxed (Unbox)
import GHC.Records

import Loomeric.Data.Vector
import Loomeric.Algebra.Group
import Loomeric.Algebra.Ring
import Loomeric.Algebra.Module
import Loomeric.Algebra.Field
import Loomeric.Algebra.VectorSpace

newtype Quaternion a = Quaternion { unQuaternion :: Vector4D a }

pattern QuaternionSV :: Unbox a => a -> Vector3D a -> Quaternion a
pattern QuaternionSV s v <- (vectorUncons . unQuaternion -> (s,v)) where
    QuaternionSV s v = Quaternion $ vectorCons s v

deriving instance (Show a, Unbox a) => Show (Quaternion a)
deriving instance (Eq a, Unbox a) => Eq (Quaternion a)
type instance Element (Quaternion a) = a
deriving instance Unbox a => MonoFunctor (Quaternion a)
deriving instance Unbox a => MonoFoldable (Quaternion a)

instance (AdditiveSemigroup a, Unbox a) => AdditiveSemigroup (Quaternion a) where
    Quaternion x + Quaternion y = Quaternion (x + y)

instance (AdditiveMonoid a, Unbox a) => AdditiveMonoid (Quaternion a) where
    zero = Quaternion zero

instance (AdditiveGroup a, Unbox a) => AdditiveGroup (Quaternion a) where
    QuaternionSV s1 v1 - QuaternionSV s2 v2 = QuaternionSV (s1 - s2) (v1 - v2)
    negate (Quaternion x) = Quaternion (negate x)

instance (Semiring a, Unbox a) => SemiModule a (Quaternion a)
instance (Ring a, Unbox a) => Module a (Quaternion a)
instance (ExponentialField a, Unbox a) => Normed a (Quaternion a) where
    normSquared x = ofoldl1Ex' (+) $ omap pow2 x where
        pow2 x = x * x
    norm = sqrt . normSquared

instance (ExponentialField a, Unbox a, Eq a) => MultiplicativeSemigroup (Quaternion a) where
    QuaternionSV s1 v1 * QuaternionSV s2 v2 = QuaternionSV (s1 * s2 - v1 `dot` v2) (s1 *^ v2 + s2 *^ v1 + v1 `cross` v2)

instance (ExponentialField a, Unbox a, Eq a) => MultiplicativeMonoid (Quaternion a) where
    one = QuaternionSV one zero

instance (ExponentialField a, Unbox a,  Eq a) => MultiplicativeGroup (Quaternion a) where
    inverse x@(QuaternionSV s v) = QuaternionSV (s / normSquared x) (negate v ^/ normSquared x)
    x / y = x * inverse y

instance (ExponentialField a, Unbox a, Eq a) => Semiring (Quaternion a) where
    fromNatural n = QuaternionSV (fromNatural n) zero
instance (ExponentialField a, Unbox a, Eq a) => Ring (Quaternion a) where
    fromInteger n = QuaternionSV (fromInteger n) zero
instance (ExponentialField a, Unbox a, Eq a) => Field (Quaternion a) where
    fromRational n = QuaternionSV (fromRational n) zero

instance (ExponentialField a, Unbox a, Eq a) => InvolutionRing (Quaternion a) where
    conjugate (QuaternionSV s v) = QuaternionSV s (negate v)

instance Unbox a => HasField "s" (Quaternion a) a where
    getField :: Unbox a => Quaternion a -> a
    getField (QuaternionSV s _) = s

instance Unbox a => HasField "v" (Quaternion a) (Vector3D a) where
    getField (QuaternionSV _ v) = v

instance (Field a, Unbox a) => VectorSpace (Quaternion a) a [ "s", "x", "y", "z" ]

instance (Field a, Unbox a) => HasCoordinate (Quaternion a) a "s" where
    basisVec = QuaternionSV one zero
    coord = (.x) . unQuaternion

instance (Field a, Unbox a) => HasCoordinate (Quaternion a) a "x" where
    basisVec = QuaternionSV zero (Vector3D one zero zero)
    coord = (.y) . unQuaternion

instance (Field a, Unbox a) => HasCoordinate (Quaternion a) a "y" where
    basisVec = QuaternionSV zero (Vector3D zero one zero)
    coord = (.z) . unQuaternion

instance (Field a, Unbox a) => HasCoordinate (Quaternion a) a "z" where
    basisVec = QuaternionSV zero (Vector3D zero zero one)
    coord = (.w) . unQuaternion

instance (ExponentialField a, Unbox a, Eq a) => NormedVectorSpace (Quaternion a) a [ "s", "x", "y", "z" ]

instance (ExponentialField a, Unbox a, TrigonometricField a, Eq a) => ExponentialSemiring (Quaternion a) where
    x ** y = exp $ y * log x

instance (ExponentialField a, Unbox a, TrigonometricField a, Eq a) => ExponentialField (Quaternion a) where
    exp x = exp x.s *^ QuaternionSV (cos (norm x.v)) (normalizeVec x.v ^* sin (norm x.v))
    log x = QuaternionSV (log x.s) (if norm x == zero then zero else normalizeVec x.v ^* acos (x.s / norm x))
    sqrt x = x ** (one / (one + one))
