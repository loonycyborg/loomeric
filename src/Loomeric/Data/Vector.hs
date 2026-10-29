{-# LANGUAGE TypeFamilies, DataKinds, UndecidableInstances, DuplicateRecordFields #-}
module Loomeric.Data.Vector where

import Data.MonoTraversable
import Data.Containers
import GHC.TypeLits
import Data.Kind
import Data.Type.Equality
import Generics.SOP
import GHC.Records
import Data.List (zipWith)
import Data.Maybe
import Data.Bifunctor

import Prelude (fst, ($), (.), repeat, map, zip, unzip, error, Eq(..), Foldable(), Show(..), Enum(..), Bool(..))

import Loomeric.Algebra.VectorSpace
import Loomeric.Algebra.Ring
import Loomeric.Algebra.Field
import Loomeric.Algebra.Group
import Loomeric.Algebra.Module

data family Vector (n :: Nat) a

data instance Vector 2 a where
    Vector2D :: Ring a => { x :: a, y :: a } -> Vector 2 a

type Vector2D = Vector 2

instance Ring a => Generic (Vector2D a) where
    type Code (Vector2D a) = '[ '[ a, a ] ]
    from (Vector2D x y) = SOP $ Z $ I x :* I y :* Nil
    to (SOP (Z (I x :* I y :* Nil))) = Vector2D x y

deriving instance Show a => Show (Vector2D a)
deriving instance Eq a => Eq (Vector2D a)
deriving instance Foldable Vector2D
instance MonoFoldable (Vector2D a)

data instance Vector 3 a where
    Vector3D :: Ring a => { x :: a, y :: a, z :: a } -> Vector 3 a

type Vector3D = Vector 3

instance Ring a => Generic (Vector3D a) where
    type Code (Vector3D a) = '[ '[ a, a, a ] ]
    from (Vector3D x y z) = SOP $ Z $ I x :* I y :* I z :* Nil
    to (SOP (Z (I x :* I y :* I z :* Nil))) = Vector3D x y z

deriving instance Show a => Show (Vector3D a)
deriving instance Eq a => Eq (Vector3D a)
deriving instance Foldable Vector3D
instance MonoFoldable (Vector3D a)

data instance Vector 4 a where
    Vector4D :: Ring a => { x :: a, y :: a, z :: a, w :: a } -> Vector 4 a

type Vector4D = Vector 4

instance Ring a => Generic (Vector4D a) where
    type Code (Vector4D a) = '[ '[ a, a, a, a ] ]
    from (Vector4D x y z w) = SOP $ Z $ I x :* I y :* I z :* I w:* Nil
    to (SOP (Z (I x :* I y :* I z :* I w :* Nil))) = Vector4D x y z w

deriving instance Show a => Show (Vector4D a)
deriving instance Eq a => Eq (Vector4D a)
deriving instance Foldable Vector4D
instance MonoFoldable (Vector4D a)

type VectorCode :: Nat -> Type -> [ Type ]
type family VectorCode n a where
    VectorCode 0 _ = '[]
    VectorCode n a = a : VectorCode (n - 1) a

type GenericVector :: Nat -> Type -> Constraint
type GenericVector n a = (Generic (Vector n a), Code (Vector n a) ~ '[ VectorCode n a ],
    AllZip (LiftedCoercible I (K a)) (VectorCode n a) (VectorCode n a),
    AllZip (LiftedCoercible (K a) I) (VectorCode n a) (VectorCode n a),
    All AdditiveMonoid (VectorCode n a)
    )

instance (Ring a, GenericVector n a) => MonoFunctor (Vector n a) where
    omap f x = to $ case from x of
        a -> htoI $ hmap (mapKK f) (hfromI a :: SOP (K a) '[VectorCode n a])

instance (Ring a, GenericVector n a) => MonoZip (Vector n a) where
    ozip x y = case (from x, from y) of
        (sop1, sop2) -> zip
            (hcollapse (hfromI sop1 :: SOP (K a) '[ VectorCode n a ]))
            (hcollapse (hfromI sop2 :: SOP (K a) '[ VectorCode n a ]))
    ozipWith f x y = to $ case (from x, from y) of
        (SOP (Z pop1), SOP (Z pop2)) -> SOP $ Z $ htoI $ hliftA2 (mapKKK f)
            (hfromI pop1)
            (hfromI pop2 :: NP (K a) (VectorCode n a))
    ounzip l = bimap mkVec mkVec $ unzip l where
        mkVec = to . SOP . Z . htoI . fromMaybe err . fromList @(VectorCode n a)
        err = error "Wrong number of elements to make vector"

type instance Element (Vector n a) = a
instance (AdditiveSemigroup a, MonoZip (Vector n a)) => AdditiveSemigroup (Vector n a) where
    x + y = ozipWith (+) x y

instance (Ring a, GenericVector n a) => AdditiveMonoid (Vector n a) where
    zero = to $ SOP $ Z $ hcpure (Proxy @AdditiveMonoid) (I zero)

instance (Ring a, GenericVector n a, MonoZip (Vector n a)) => AdditiveGroup (Vector n a) where
    negate = omap negate
    x - y = ozipWith (-) x y

instance (Ring a, GenericVector n a, MonoZip (Vector n a)) => SemiModule a (Vector n a)
instance (Ring a, GenericVector n a, MonoZip (Vector n a)) => Module a (Vector n a)

instance Field a => VectorSpace (Vector2D a) a ["x", "y"]
instance Field a => VectorSpace (Vector3D a) a ["x", "y", "z"]
instance Field a => VectorSpace (Vector4D a) a ["x", "y", "z", "w"]

type CoordNum :: Symbol -> Nat
type family CoordNum s where
    CoordNum "x" = 0
    CoordNum "y" = 1
    CoordNum "z" = 2
    CoordNum "w" = 3

instance (Ring a, GenericVector n a, coordN ~ CoordNum s, HasField s (Vector n a) a, KnownNat coordN, KnownNat n) => HasCoordinate (Vector n a) a (s :: Symbol) where
    basisVec = let dim_coord = natVal $ Proxy @coordN
                   max_coord = natVal $ Proxy @n
                   bool2a :: Bool -> a
                   bool2a True  = one
                   bool2a False = zero
               in to . SOP . Z . htoI . fromJust . fromList @(VectorCode n a) $ map (bool2a . (==dim_coord)) [0 .. max_coord - 1]
    coord = getField @s

instance (GenericVector n a, ExponentialField a) => Normed a (Vector n a) where
    normSquared x = linearCombination $ ozip x x
    norm = sqrt . normSquared 

instance (GenericVector n a, VectorSpace (Vector n a) a c, ExponentialField a, Eq a) => NormedVectorSpace (Vector n a) a c

instance (GenericVector n a, NormedVectorSpace (Vector n a) a c) => InnerProductSpace (Vector n a) a c where
    dot x y = linearCombination $ ozip x y
    cross = error "Cross product not defined"

instance {-# OVERLAPS #-} (GenericVector 3 a, NormedVectorSpace (Vector3D a) a '["x", "y", "z"]) => InnerProductSpace (Vector3D a) a '["x", "y", "z"] where
    dot x y = linearCombination $ ozip x y
    cross (Vector3D x1 x2 x3) (Vector3D y1 y2 y3) = Vector3D (x2*y3 - x3*y2) (x3*y1 - x1*y3) (x1*y2 - x2*y1)

toVector :: forall v s coords n . (
    Ring s, GenericVector n s, VectorSpace v s coords, n ~ Dimension coords,
    AllZip (LiftedCoercible (K s) I) coords (VectorCode n s)
    ) => v -> Vector n s
toVector x = to $ SOP $ Z $ htoI (hcpure (Proxy @(HasCoordinate v s)) f :: NP (K s) coords) where
        f :: forall (n :: Symbol) . HasCoordinate v s n => K s n
        f = K (coord @v @s @n x)

fromVector :: forall v s coords n cn . (GenericVector n s, VectorSpace v s coords, VectorSpace (Vector n s) s cn, n ~ Dimension coords) => Vector n s -> v
fromVector x = sum $ zipWith (*^) (coords x) basis

newtype Swizzle (permutation :: [Symbol]) n a = Swizzle { unSwizzle :: Vector n a }

deriving instance Show (Vector n a) => Show (Swizzle permutation n a)
deriving instance Eq (Vector n a) => Eq (Swizzle permutation n a)
instance (Ring a, GenericVector n a) => AdditiveSemigroup (Swizzle permutation n a) where
    x + y = Swizzle $ unSwizzle x + unSwizzle y

instance (Ring a, GenericVector n a) => AdditiveMonoid (Swizzle permutation n a) where
    zero = Swizzle zero

deriving instance (Ring a, GenericVector n a) => AdditiveGroup (Swizzle permutation n a)
deriving instance (Ring a, GenericVector n a, MonoZip (Vector n a)) => SemiModule a (Swizzle permutation n a)
deriving instance (Ring a, GenericVector n a, MonoZip (Vector n a)) => Module a (Swizzle permutation n a)

type family PermutationCoords permutation where
    PermutationCoords '[ _ ]          = '["x"]
    PermutationCoords '[ _, _ ]       = '["x", "y"]
    PermutationCoords '[ _, _, _ ]    = '["x", "y", "z"]
    PermutationCoords '[ _, _, _, _ ] = '["x", "y", "z", "w"]

type family PermutationPreimage permutation n where
    PermutationPreimage (x:_)  0 = x
    PermutationPreimage (_:xs) n = PermutationPreimage xs (n-1)

instance (Field a, 1 <= Dimension coords,
    coords ~ PermutationCoords permutation, Dimension coords ~ Dimension permutation,
    All (HasCoordinate (Swizzle permutation n a) a) coords,
    GenericVector n a) => VectorSpace (Swizzle permutation n a) a coords

instance (
        CoordNum c <= n - 1, GenericVector n a, Ring a, KnownNat n,
        HasField (PermutationPreimage permutation (CoordNum c)) (Vector n a) a,
        KnownNat (CoordNum (PermutationPreimage permutation (CoordNum c)))
    ) => HasCoordinate (Swizzle permutation n a) a c where
    basisVec = Swizzle $ basisVec @(Vector n a) @a @(PermutationPreimage permutation (CoordNum c))
    coord x = coord @(Vector n a) @a @(PermutationPreimage permutation (CoordNum c)) $ unSwizzle x
