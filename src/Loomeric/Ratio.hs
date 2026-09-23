module Loomeric.Ratio ((%), numerator, denominator, Ratio, Rational) where
import Loomeric.Group

import Prelude (($), (.), (>), undefined, Show(..), Eq(..), showParen, showString, Integer)

data Ratio a where
    (:%) :: EuclideanDomain a => a -> a -> Ratio a

deriving instance Eq (Ratio a)

type Rational = Ratio Integer

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
