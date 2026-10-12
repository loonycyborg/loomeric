{-# LANGUAGE UndecidableInstances, UnboxedTuples, MagicHash #-}
module Loomeric.Data.Conversions.Rational where

import GHC.Exts
import GHC.Base ( compareInt# )

import Loomeric.Algebra.Group
import Loomeric.Algebra.Ring
import Loomeric.Data.Ratio
import Loomeric.Data.Conversions.Integer

import Prelude (($), (<$>), id, error)

class ToRational a where
    toRational :: a -> Rational

instance {-# OVERLAPPABLE #-} (ToInteger a) => ToRational a where
    toRational = (% one) <$> toInteger

instance ToRational Float where
    toRational (F# x) = case decodeFloat_Int# x of
        (# m, e #) -> case compareInt# e 0# of
            EQ -> toInteger (I# m) % one
            GT -> toInteger (I# m * toSigned exp) % one
            LT -> toInteger (I# m) % toInteger (toSigned exp)
            where exp = 2 ** absG (I# e)

instance ToRational Rational where
    toRational = id
