{-# LANGUAGE AllowAmbiguousTypes, TypeAbstractions, UndecidableInstances #-}
module Loomeric.Algebra.PartialAction where

import Data.Maybe
import Data.Bifunctor
import Control.Applicative
import Control.Monad
import GHC.Num.Natural
import GHC.Num.Integer
import GHC.Float (Float(..))
import Data.Type.Equality

import Prelude (($), (.), (==), (&&), const, id, error, uncurry, any, Bool(..), Word(..), Int(..), Ord(..), Ordering(..))

import Loomeric.Algebra.Group
import Loomeric.Algebra.Ring
import Loomeric.Algebra.Field
import Loomeric.Algebra.Partial
import Loomeric.Data.Conversions.Partial
import Loomeric.Data.Conversions.Integer
import Loomeric.Data.Ratio
import Loomeric.Data.Integer

infixr 8 **?, **!
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

class Semiring a => ExponentialPartialAction a where
    (**?) :: Countable b => a -> b -> Maybe a
    (**!) :: Countable b => a -> b -> a
    x **! y = fromJust $ x **? y <|> error "Invalid exponentiation"

instance ExponentialPartialAction Word where
    (**?) = expActionUnsigned

instance ExponentialPartialAction Natural where
    (**?) = expActionUnsigned

expActionUnsigned :: (Countable b, ExponentialSemiring a, OrderedSemiring a, AdditivePartialGroup a, EuclideanDomain a) => a -> b -> Maybe a
expActionUnsigned x y = case toNumber y of
        (_, _, Just n) -> Just $ x ** fromNatural n
        (Just r, _, _) -> if r >= zero then rationalExp x r else Nothing
        _              -> Nothing

naturalRoot :: (ExponentialSemiring a, OrderedSemiring a, AdditivePartialGroup a, EuclideanDomain a) => a -> a -> Maybe a
naturalRoot operand degree = if operand == zero then Just zero else bisect one operand where
    bisect from to = if to == from then Nothing else
        let try = from + quot (to -! from) (one + one) in
            case compare (try ** degree) operand of
                EQ -> Just try
                GT -> bisect from try
                LT -> bisect (try + one) to

rationalExp :: (ExponentialSemiring a, AdditivePartialGroup a,  EuclideanDomain a) => a -> Ratio Integer -> Maybe a
rationalExp x (r :/ r') = (** fromNatural (absG r)) <$> naturalRoot x (fromNatural $ absG r')

instance ExponentialPartialAction Int where
    (**?) = expActionSigned @Int @_ @Word

instance ExponentialPartialAction Integer where
    (**?) = expActionSigned @Integer @_ @Natural

expActionSigned :: forall a b u . (
    Countable b, SignConvert u, a ~ SignedType u, SignTruncate u,
    ExponentialPartialAction u, ExponentialSemiring u, AdditivePartialGroup u, EuclideanDomain u,
    EuclideanDomain a) => a -> b -> Maybe a
expActionSigned @a @b @u x y = case toNumber y of
        (_, _, Just n) -> (sign n *) . signConvert @u <$> (absG x **? n)
        (Just r, _, _) -> if r >= zero then imaginary_root (denominator r) . (sign (numerator r) *) . signConvert @u =<< rationalExp (absG x) r else Nothing
        _              -> Nothing
        where sign n = if odd n then signum x else one
              imaginary_root degree = if even degree && x < zero then const Nothing else Just

instance (SignConvert u, a ~ SignedType u, SignTruncate u,
    ExponentialPartialAction u, ExponentialSemiring u, AdditivePartialGroup u, EuclideanDomain u,
    EuclideanDomain a) => ExponentialPartialAction (Ratio a) where
    (x :/ x') **? y = case toNumber y of
        (Just r, _, _) -> liftM2 (%) (expActionSigned @a @_ @u x r) (expActionSigned @a @_ @u x' r)
        _                       -> Nothing
    (x :/ x') **! y = case toNumber y of
        (Just r, _, _) -> fromJust $ liftM2 (%) (expActionSigned @a @_ @u x r) (expActionSigned @a @_ @u x' r) <|> error "Invalid exponentiation"
        _                       -> error "not a rational"
