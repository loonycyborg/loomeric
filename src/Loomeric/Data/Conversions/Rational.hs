{-# LANGUAGE UndecidableInstances, UnboxedTuples, MagicHash #-}
module Loomeric.Data.Conversions.Rational where

import GHC.Float
import GHC.Exts

import Loomeric.Algebra.Group
import Loomeric.Data.Ratio
import Loomeric.Data.Conversions.Integer

import Prelude ((<$>), id, error)

class ToRational a where
    toRational :: a -> Rational

instance (ToInteger a) => ToRational a where
    toRational = (% one) <$> toInteger

instance ToRational Float where
    toRational (F# x) = case decodeFloat_Int# x of
        (# _, -0xFF# #) -> error "Infinite or NAN float detected"

instance ToRational Rational where
    toRational = id
