{-# LANGUAGE MagicHash, CPP #-}

{-|
    Module      :  Data.Number.MPFR.Down
    Description :  top level
    Copyright   :  (c) Aleš Bizjak
    License     :  BSD3

    Maintainer  :  ales.bizjak0@gmail.com
    Stability   :  experimental
    Portability :  non-portable

  This module defines instances Num, Real, Fractional, Floating and RealFrac of MPFR.
  Operations are rounded with RoundMode Down and computed with max precision of two 
  operands or with the precision of the operand. Otherwise it is equivalent to 
  Data.Number.MPFR
-}
{-# INCLUDE <mpfr.h> #-}
{-# INCLUDE <chsmpfr.h> #-}

module Data.Number.MPFR.Down (
       module Data.Number.MPFR.Base 
)
where

import Data.Number.MPFR.Base

import Data.Number.MPFR.Internal

import Data.Maybe

import Data.Ratio

#if __GLASGOW_HASKELL__ >= 610
import GHC.Integer.Internals
import GHC.Exts hiding (Down)
#else 
import GHC.Exts
#endif


instance Num MPFR where
    d + d'        = add Down (maxPrec d d') d d'
    d - d'        = sub Down (maxPrec d d') d d'
    d * d'        = mul Down (maxPrec d d') d d'
    negate d      = neg Down (getPrec d) d
    abs d         = absD Down (getPrec d) d
    signum        = fromInt Down minPrec . fromMaybe (-1) .sgn
    fromInteger (S# i) = fromInt Down minPrec (I# i)
    fromInteger i@(J# n _) = fromIntegerA Zero (fromIntegral . abs $ I# n * bitsPerIntegerLimb) i 

instance Real MPFR where
    toRational d = n % 2 ^ e
        where (n', e') = decompose d
              (n, e) = if e' >= 0 then ((n' * 2 ^ e'), 0)
                         else (n', - e')

instance Fractional MPFR where
    d / d'         = Data.Number.MPFR.Base.div Down (maxPrec d d') d d'
    fromRational r = fromInteger n / fromInteger d
        where n = numerator r
              d = denominator r
    recip d        = one / d

instance Floating MPFR where
    pi           = Data.Number.MPFR.Base.pi Down 53
    exp d        = Data.Number.MPFR.Base.exp Down (getPrec d) d
    log d        = Data.Number.MPFR.Base.log Down (getPrec d) d
    sqrt d       = Data.Number.MPFR.Base.sqrt Down (getPrec d) d 
    (**) d d'    = Data.Number.MPFR.Base.pow Down (maxPrec d d') d d'
    logBase d d' = Prelude.log d' / Prelude.log d
    sin d        = Data.Number.MPFR.Base.sin Down (getPrec d) d
    cos d        = Data.Number.MPFR.Base.cos Down (getPrec d) d
    tan d        = Data.Number.MPFR.Base.tan Down (getPrec d) d
    asin d       = Data.Number.MPFR.Base.asin Down (getPrec d) d
    acos d       = Data.Number.MPFR.Base.acos Down (getPrec d) d
    atan d       = Data.Number.MPFR.Base.atan Down (getPrec d) d
    sinh d       = Data.Number.MPFR.Base.sinh Down (getPrec d) d
    cosh d       = Data.Number.MPFR.Base.cosh Down (getPrec d) d
    tanh d       = Data.Number.MPFR.Base.tanh Down (getPrec d) d
    asinh d      = Data.Number.MPFR.Base.asinh Down (getPrec d) d
    acosh d      = Data.Number.MPFR.Base.acosh Down (getPrec d) d
    atanh d      = Data.Number.MPFR.Base.atanh Down (getPrec d) d

instance RealFrac MPFR where
    properFraction d = (fromIntegral n, f)
        where r = toRational d
              m = numerator r
              e = denominator r
              n = quot m e
              f = frac Down (getPrec d) d
