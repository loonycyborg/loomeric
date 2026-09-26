{-# LANGUAGE TypeFamilies #-}
module Loomeric.Ratio ((%), numerator, denominator, Ratio, Rational) where
import Control.Applicative
import Data.MonoTraversable

import Loomeric.Group
import Loomeric.Integer
import Loomeric.Ring

import Prelude (($), (.), (>), undefined, Show(..), Eq(..), Ord(..), Foldable(..), showParen, showString, Integer)

data Ratio a where
    (:%) :: EuclideanDomain a => a -> a -> Ratio a

deriving instance Eq (Ratio a)
deriving instance Foldable Ratio

instance Ord (Ratio a) where
    compare (x :% x') (y :% y') = compare (x * y') (y * x')

type Rational = Ratio Integer

type instance Element (Ratio a) = a

instance MonoFunctor (Ratio a) where
    omap f (x :% x') = normalize $ f x :% f x'

instance MonoFoldable (Ratio a)
instance MonoTraversable (Ratio a) where
    otraverse f (x :% x') = normalize <$> liftA2 (:%) (f x) (f x')

(%) :: EuclideanDomain a => a -> a -> Ratio a
x % y = normalize $ x :% y

infixl 7 %

normalize :: Ratio a -> Ratio a
normalize (x :% y) = (signum y * abs x `quot` g) :% (abs y `quot` g) where
    g = gcd x y

numerator :: Ratio a -> a
numerator   (x :% _) = x

denominator :: Ratio a -> a
denominator (_ :% x) = x

instance AdditiveSemigroup (Ratio a) where
    (x :% x') + (y :% y') = normalize $ (x * y' + y * x') :% (x' * y')

instance EuclideanDomain a => AdditiveMonoid (Ratio a) where
    zero = zero % one

instance (EuclideanDomain a, AdditiveGroup a) => AdditiveGroup (Ratio a) where
    (x :% x') - (y :% y') = normalize $ (x * y' - y * x') :% (x' * y')
    negate (x :% x') = negate x :% x'

instance MultiplicativeSemigroup (Ratio a) where
    (x :% x') * (y :% y') = normalize $ (x * y) :% (x' * y')

instance EuclideanDomain a => MultiplicativeMonoid (Ratio a) where
    one = one :% one

instance EuclideanDomain a => MultiplicativeGroup (Ratio a) where
    inverse (x :% x') = normalize $ x' :% x
    (x :% x') / (y :% y') = normalize $ (x * y') :% (x' * y)

instance EuclideanDomain a => Semiring (Ratio a) where
    fromNatural n = fromNatural n :% one

instance (EuclideanDomain a, Ring a) => Ring (Ratio a) where
    fromInteger n = fromInteger n :% one

instance  (Show a)  => Show (Ratio a)  where
    showsPrec p (x:%y)  =  showParen (p > 7) $
                           showsPrec 8 x .
                           showString " % " .
                           showsPrec 8 y
