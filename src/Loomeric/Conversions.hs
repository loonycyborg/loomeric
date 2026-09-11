{-# LANGUAGE TypeFamilies, MagicHash, DefaultSignatures #-}
module Loomeric.Conversions where
import Prelude (($), id, Int, Word)
import GHC.Num.Integer
import GHC.Num.Natural
import GHC.Natural
import Data.Kind
import GHC.Exts

class ToInteger a where
    toInteger :: a -> Integer

instance ToInteger Natural where
    toInteger = naturalToInteger

instance ToInteger Int where
    toInteger = integerFromInt

instance ToInteger Word where
    toInteger = integerFromWord

type SignedType :: Type -> Type
type family SignedType a where
    SignedType Word    = Int
    SignedType Natural = Integer
    SignedType a       = a

class SignConvert a where
    signConvert :: a -> SignedType a
    default signConvert :: a ~ SignedType a => a -> SignedType a
    signConvert = id

instance SignConvert Word where
    signConvert (W# x) = I# $ word2Int# x

instance SignConvert Natural where
    signConvert = naturalToInteger

instance SignConvert Int
instance SignConvert Integer

class SignTruncate a where
    signTruncate :: SignedType a -> a
    default signTruncate :: SignedType a ~ a => SignedType a -> a
    signTruncate = id

instance SignTruncate Word where
    signTruncate (I# x) = W# $ int2Word# x

instance SignTruncate Natural where
    signTruncate = naturalFromInteger
