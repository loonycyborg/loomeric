module Loomeric.Algebra.PartialAction where

import Data.Maybe
import Data.Bifunctor
import Control.Applicative
import Control.Monad
import GHC.Num.Natural
import GHC.Num.Integer

import Prelude (($), (.), error, uncurry, any, Bool(..), Word(..), Int(..), Ord(..))

import Loomeric.Algebra.Group
import Loomeric.Algebra.Ring
import Loomeric.Algebra.Field
import Loomeric.Algebra.Partial
import Loomeric.Data.Conversions.Partial
import Loomeric.Data.Conversions.Integer
import Loomeric.Data.Ratio
import Loomeric.Data.Integer

infixl 7 *?, *!
infixl 6 +?, +!

class AdditivePartialAction a where
    (+?) :: Countable b => a -> b -> Maybe a
    (+!) :: Countable b => a -> b -> a
    x +! y = fromJust $ (x +? y) <|> error "Invalid addition"

instance AdditivePartialAction Word where
    (+?) = addActionUnsigned

instance AdditivePartialAction Natural where
    (+?) = addActionUnsigned

addActionUnsigned :: (Semiring a, AdditivePartialGroup a, Countable b) => a -> b -> Maybe a
addActionUnsigned x y = case toNumber y of
        (_, _, Just n) -> Just $ x + fromNatural n
        (_, sn, _)     -> (x-?) . fromNatural . signTruncate =<< sn

instance AdditivePartialAction Int where
    (+?) = addActionSigned

instance AdditivePartialAction Integer where
    (+?) = addActionSigned

addActionSigned :: (Ring a, Countable b) => a -> b -> Maybe a
addActionSigned x y = case toNumber y of
    (_, sn, _) -> (x+) . fromInteger <$> sn

instance (EuclideanDomain a, Ring a) => AdditivePartialAction (Ratio a) where
    x +? y = case toNumber y of
        (r, _, _) -> (x+) . fromRational <$> r

class MultiplicativePartialAction a where
    (*?) :: Countable b => a -> b -> Maybe a
    (*!) :: Countable b => a -> b -> a
    x *! y = fromJust $ (x *? y) <|> error "Invalid multiplication"

instance MultiplicativePartialAction Natural where
    (*?) = mulActionUnsigned

instance MultiplicativePartialAction Word where
    (*?) = mulActionUnsigned

mulActionUnsigned :: (Semiring a, Ring (SignedType a), MultiplicativePartialGroup a, SignTruncate a, Countable b) => a -> b -> Maybe a
mulActionUnsigned x y = case toNumber y of
    (_, _, Just n) -> Just $ x * fromNatural n
    (r, _, _)      -> if any (>= zero) r then
        liftM2 (,) r r >>= uncurry (/?) . bimap ((x*) . signTruncate . fromInteger . numerator) (signTruncate . fromInteger . denominator)
        else Nothing

instance MultiplicativePartialAction Int where
    (*?) = mulActionSigned

instance MultiplicativePartialAction Integer where
    (*?) = mulActionSigned

mulActionSigned :: (Ring a, MultiplicativePartialGroup a, SignTruncate a, Countable b) => a -> b -> Maybe a
mulActionSigned x y = case toNumber y of
    (_, _, Just  n) -> Just $ x * fromNatural n
    (_, Just sn, _) -> Just $ x * fromInteger sn
    (r, _,  _)      -> liftM2 (,) r r >>= uncurry (/?) . bimap ((x*) . fromInteger . numerator) (fromInteger . denominator)

instance (EuclideanDomain a, Ring a) => MultiplicativePartialAction (Ratio a) where
    x *? y = case toNumber y of
        (r, _, _) -> (x*) . fromRational <$> r
