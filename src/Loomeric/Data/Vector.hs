{-# LANGUAGE TypeFamilyDependencies, PatternSynonyms, ViewPatterns, DataKinds, RequiredTypeArguments, MagicHash, DerivingStrategies, UndecidableInstances, DuplicateRecordFields #-}
module Loomeric.Data.Vector where

import Data.MonoTraversable
import Data.Containers
import GHC.TypeLits
import GHC.Exts
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector.Unboxed as Unboxed
import Data.Type.Equality
import Generics.SOP
import GHC.Records
import Data.List (zipWith)

import Prelude (fst, ($), (.), const, uncurry, repeat, all, map, mconcat, zip, unzip, error, Eq(..), Foldable(foldr), Show(..), Enum(..), Bool(..))

import Loomeric.Algebra.VectorSpace
import Loomeric.Algebra.Ring
import Loomeric.Algebra.Field
import Loomeric.Algebra.Group
import Loomeric.Algebra.Module

newtype Vector (n :: Nat) a = Vector (Unboxed.Vector a) deriving Show
type Vector2D a = Vector 2 a
type Vector3D a = Vector 3 a
type Vector4D a = Vector 4 a

pattern Vector2D :: Unbox a => a -> a -> Vector 2 a
pattern Vector2D x y <- (otoList -> [x, y]) where
    Vector2D x y = vector 2 [x, y]
pattern Vector3D :: Unbox a => a -> a -> a -> Vector 3 a
pattern Vector3D x y z <- (otoList -> [x, y, z]) where
    Vector3D x y z = vector 3 [x, y, z]
pattern Vector4D :: Unbox a => a -> a -> a -> a -> Vector 4 a
pattern Vector4D x y z w <- (otoList -> [x, y, z, w]) where
    Vector4D x y z w = vector 4 [x, y, z, w]

instance (Eq a, Unbox a, KnownNat n, KnownNat (n-1)) => Eq (Vector n a) where
    x == y = all coord_eq [0..fromInteger $ natVal' (proxy# @(n-1))] where
        coord_eq i = vectorIndexUnsafe x i == vectorIndexUnsafe y i

deriving newtype instance Unbox a => MonoFoldable (Vector n a)

vector :: forall n -> (Unbox a, KnownNat n) => [a] -> Vector n a
vector n = Vector . check_length . Unboxed.fromListN (fromInteger $ natVal' (proxy# @n)) where
    check_length x = if Unboxed.length x == fromInteger (natVal' (proxy# @n)) then x else error "Not enough elements in the input list to make the vector"

vectorGenerate :: forall n a . (Unbox a, KnownNat n, KnownNat (n-1)) => (Int -> a) -> Vector n a
vectorGenerate f = Vector $
    Unboxed.generate (fromInteger $ natVal' (proxy# @n)) f

vectorIndexUnsafe :: Unbox a => Vector n a -> Int -> a
vectorIndexUnsafe (Vector x) = Unboxed.unsafeIndex x

vectorIndex :: forall (i :: Nat) -> (Unbox a, KnownNat i) => Vector n a -> a
vectorIndex i x = vectorIndexUnsafe x (fromInteger $ natVal' (proxy# @i))

vectorCons :: Unbox a => a -> Vector (n-1) a -> Vector n a
vectorCons a (Vector x) = Vector $ Unboxed.cons a x

vectorUncons :: Unbox a => Vector n a -> (a, Vector (n-1) a)
vectorUncons (Vector x) = (Unboxed.unsafeHead x, Vector $ Unboxed.unsafeTail x)

instance Unbox a => MonoFunctor (Vector n a) where
    omap f (Vector a) = Vector $ Unboxed.map f a

instance (Unbox a, KnownNat n, KnownNat (n-1)) => MonoZip (Vector n a) where
    ozip x y = [(vectorIndexUnsafe x i, vectorIndexUnsafe y i) | i <- [0..fromInteger $ natVal' (proxy# @(n-1))]]
    ozipWith f x y = vectorGenerate $ \i -> f (vectorIndexUnsafe x i) (vectorIndexUnsafe y i)
    ounzip l = (vector n ls, vector n rs) where
        (ls, rs) = unzip l

type instance Element (Vector n a) = a
instance (AdditiveSemigroup a, MonoZip (Vector n a)) => AdditiveSemigroup (Vector n a) where
    x + y = ozipWith (+) x y

instance (AdditiveMonoid a, Unbox a, KnownNat n, KnownNat (n-1)) => AdditiveMonoid (Vector n a) where
    zero = vectorGenerate $ const zero

instance (AdditiveGroup a, Unbox a, KnownNat n, KnownNat (n-1)) => AdditiveGroup (Vector n a) where
    negate = omap negate
    x - y = ozipWith (-) x y

instance (Semiring a, Unbox a, KnownNat n, KnownNat (n-1)) => SemiModule a (Vector n a)
instance (Ring a, Unbox a, KnownNat n, KnownNat (n-1)) => Module a (Vector n a)

instance (Field a, Unbox a) => VectorSpace (Vector2D a) a ["x", "y"]
instance (Field a, Unbox a) => VectorSpace (Vector3D a) a ["x", "y", "z"]
instance (Field a, Unbox a) => VectorSpace (Vector4D a) a ["x", "y", "z", "w"]

instance (CoordNum c <= n-1, Ring s, Unbox s, KnownNat n, KnownNat (n-1), KnownNat (CoordNum c)) => HasField c (Vector n s) s where
    getField = coord @_ @_ @c

type CoordNum :: Symbol -> Nat
type family CoordNum s = r | r -> s where
    CoordNum "x" = 0
    CoordNum "y" = 1
    CoordNum "z" = 2
    CoordNum "w" = 3

instance (Ring a, coordN ~ CoordNum s, KnownNat coordN, Unbox a, KnownNat n, KnownNat (n-1)) => HasCoordinate (Vector n a) a (s :: Symbol) where
    basisVec = vectorGenerate $ \i -> if i == fromInteger (natVal' (proxy# @coordN)) then one else zero
    coord = vectorIndex coordN

instance (Unbox a, KnownNat n, KnownNat (n-1), ExponentialField a) => Normed a (Vector n a) where
    normSquared x = linearCombination $ ozip x x
    norm = sqrt . normSquared

instance (Unbox a, KnownNat n, KnownNat (n-1), VectorSpace (Vector n a) a c, ExponentialField a, Eq a) => NormedVectorSpace (Vector n a) a c

instance (Unbox a, KnownNat n, KnownNat (n-1), NormedVectorSpace (Vector n a) a c) => InnerProductSpace (Vector n a) a c where
    dot x y = linearCombination $ ozip x y
    cross = error "Cross product not defined"

instance {-# OVERLAPS #-} (NormedVectorSpace (Vector3D a) a '["x", "y", "z"], Unbox a) => InnerProductSpace (Vector3D a) a '["x", "y", "z"] where
    dot x y = linearCombination $ ozip x y
    cross (Vector3D x1 x2 x3) (Vector3D y1 y2 y3) = Vector3D (x2*y3 - x3*y2) (x3*y1 - x1*y3) (x1*y2 - x2*y1)

toVector :: forall v s coords n . (Unbox s, VectorSpace v s coords, Dimension coords ~ n, KnownNat n) => v -> Vector n s
toVector x = vector n $ coords x

fromVector :: forall v s coords n cn . (VectorSpace v s coords, VectorSpace (Vector n s) s cn, n ~ Dimension coords) => Vector n s -> v
fromVector x = sum $ zipWith (*^) (coords x) basis

newtype Swizzle (permutation :: [Symbol]) n a = Swizzle { unSwizzle :: Vector n a }

deriving instance Show (Vector n a) => Show (Swizzle permutation n a)
deriving instance Eq (Vector n a) => Eq (Swizzle permutation n a)
instance (Ring a, Unbox a, KnownNat n, KnownNat (n-1)) => AdditiveSemigroup (Swizzle permutation n a) where
    x + y = Swizzle $ unSwizzle x + unSwizzle y

instance (Ring a, Unbox a, KnownNat n, KnownNat (n-1)) => AdditiveMonoid (Swizzle permutation n a) where
    zero = Swizzle zero

deriving newtype instance (Ring a, Unbox a, KnownNat n, KnownNat (n-1)) => AdditiveGroup (Swizzle permutation n a)
deriving newtype instance (Ring a, Unbox a, KnownNat n, KnownNat (n-1), MonoZip (Vector n a)) => SemiModule a (Swizzle permutation n a)
deriving newtype instance (Ring a, Unbox a, KnownNat n, KnownNat (n-1), MonoZip (Vector n a)) => Module a (Swizzle permutation n a)

type family PermutationCoords permutation where
    PermutationCoords '[ _ ]          = '["x"]
    PermutationCoords '[ _, _ ]       = '["x", "y"]
    PermutationCoords '[ _, _, _ ]    = '["x", "y", "z"]
    PermutationCoords '[ _, _, _, _ ] = '["x", "y", "z", "w"]

type family PermutationPreimage permutation n where
    PermutationPreimage (x:_)  0 = x
    PermutationPreimage (_:xs) n = PermutationPreimage xs (n-1)

instance forall a n permutation coords . (Unbox a, KnownNat n, KnownNat (n-1), Field a, 1 <= Dimension coords,
    coords ~ PermutationCoords permutation, Dimension coords ~ Dimension permutation,
    All (HasCoordinate (Swizzle permutation n a) a) coords
    ) => VectorSpace (Swizzle permutation n a) a coords

instance (
    Ring a, Unbox a, KnownNat n, KnownNat (n-1),
    HasField (PermutationPreimage permutation (CoordNum c)) (Vector n a) a,
    KnownNat (CoordNum (PermutationPreimage permutation (CoordNum c)))
    ) => HasCoordinate (Swizzle permutation n a) a c where
    basisVec = Swizzle $ basisVec @(Vector n a) @a @(PermutationPreimage permutation (CoordNum c))
    coord x = coord @(Vector n a) @a @(PermutationPreimage permutation (CoordNum c)) $ unSwizzle x
