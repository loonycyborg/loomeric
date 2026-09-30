{-# LANGUAGE TypeFamilies, DataKinds, UndecidableInstances #-}
module Loomeric.Data.Vector where

import Control.Applicative
import Data.MonoTraversable
import Data.Containers
import GHC.TypeNats

import Prelude (fst, ($), repeat, Eq(..), Foldable(..), Show(..))

import Loomeric.Algebra.VectorSpace
import Loomeric.Algebra.Ring
import Loomeric.Algebra.Group
import Loomeric.Algebra.Module

data family Vector (n :: Nat) a

data instance Vector 2 a where
    Vector2D :: (Ring a) => a -> a -> Vector 2 a

type Vector2D = Vector 2

deriving instance Show a => Show (Vector2D  a)
deriving instance Eq a => Eq (Vector2D a)
deriving instance Foldable Vector2D
instance MonoFoldable (Vector2D a)

instance MonoFunctor (Vector2D a) where
    omap f (Vector2D x x') = Vector2D (f x) (f x')
instance Ring a => MonoZip (Vector2D a) where
    ozip (Vector2D x x') (Vector2D y y') = [(x,y), (x',y')]
    ozipWith f (Vector2D x x') (Vector2D y y') = Vector2D (f x y) (f x' y')
    ounzip ((x, x') : (y, y') : _) = (Vector2D x y, Vector2D x' y')
instance MonoTraversable (Vector2D a) where
    otraverse f (Vector2D x x') = liftA2 Vector2D (f x) (f x')

type instance Element (Vector n a) = a
instance (AdditiveSemigroup a, MonoZip (Vector n a)) => AdditiveSemigroup (Vector n a) where
    x + y = ozipWith (+) x y

instance (Ring a, MonoZip (Vector n a)) => AdditiveMonoid (Vector n a) where
    zero = fst $ ounzip $ repeat (zero, zero)

instance (Ring a, MonoZip (Vector n a)) => AdditiveGroup (Vector n a) where
    negate = omap negate
    x - y = ozipWith (-) x y

instance (Ring a, MonoZip (Vector n a)) => SemiModule a (Vector n a)
instance (Ring a, MonoZip (Vector n a)) => Module a (Vector n a)

instance Ring a => VectorSpace ["x", "y"] a (Vector2D a)

instance Ring a => HasCoordinate "x" a (Vector2D a) where
    basisVec = Vector2D one zero
    coord (Vector2D x y) = x
instance Ring a => HasCoordinate "y" a (Vector2D a) where
    basisVec = Vector2D zero one
    coord (Vector2D x y) = y
