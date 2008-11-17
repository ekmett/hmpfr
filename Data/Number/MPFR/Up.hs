{-# LANGUAGE MagicHash, CPP #-}

{-|
    Module      :  Data.Number.MPFR.Up
    Description :  top level
    Copyright   :  (c) Aleš Bizjak
    License     :  BSD3

    Maintainer  :  ales.bizjak0@gmail.com
    Stability   :  experimental
    Portability :  non-portable

  This module defines instances Num, Real, Fractional, Floating and RealFrac of MPFR.
  Operations are rounded with RoundMode Up and computed with max precision of two 
  operands or with the precision of the operand. Otherwise it is equivalent to 
  Data.Number.MPFR
-}

{-# INCLUDE <mpfr.h> #-}
{-# INCLUDE <chsmpfr.h> #-}


module Data.Number.MPFR.Up (
       module Data.Number.MPFR.Base 
)
where

import Data.Number.MPFR.Base

import Data.Number.MPFR.Internal

import Data.Maybe

import Data.Ratio

#if __GLASGOW_HASKELL__ >= 610
import GHC.Integer.Internals
#endif
import GHC.Exts

instance Num MPFR where
    d + d'        = add Up (maxPrec d d') d d'
    d - d'        = sub Up (maxPrec d d') d d'
    d * d'        = mul Up (maxPrec d d') d d'
    negate d      = neg Up (getPrec d) d
    abs d         = absD Up (getPrec d) d
    signum d      = fromInt Up minPrec (fromMaybe (-1) (sgn d))
    fromInteger (S# i) = fromInt Up minPrec (I# i)
    fromInteger i@(J# n _) = fromIntegerA Zero (fromIntegral $ I# n * bitsPerIntegerLimb) i 

instance Real MPFR where
    toRational d = n % 2 ^ e
        where (n', e') = decompose d
              (n, e) = if e' >= 0 then ((n' * 2 ^ e'), 0)
                         else (n', - e')

instance Fractional MPFR where
    d / d'         = Data.Number.MPFR.Base.div Up (maxPrec d d') d d'
    fromRational r = (fromInteger n) / (fromInteger d)
        where n = numerator r
              d = denominator r
    recip d        = one / d

instance Floating MPFR where
    pi           = Data.Number.MPFR.Base.pi Up 53
    exp d        = Data.Number.MPFR.Base.exp Up (getPrec d) d
    log d        = Data.Number.MPFR.Base.log Up (getPrec d) d
    sqrt d       = Data.Number.MPFR.Base.sqrt Up (getPrec d) d 
    (**) d d'    = Data.Number.MPFR.Base.pow Up (maxPrec d d') d d'
    logBase d d' = Prelude.log d' / Prelude.log d
    sin d        = Data.Number.MPFR.Base.sin Up (getPrec d) d
    cos d        = Data.Number.MPFR.Base.cos Up (getPrec d) d
    tan d        = Data.Number.MPFR.Base.tan Up (getPrec d) d
    asin d       = Data.Number.MPFR.Base.asin Up (getPrec d) d
    acos d       = Data.Number.MPFR.Base.acos Up (getPrec d) d
    atan d       = Data.Number.MPFR.Base.atan Up (getPrec d) d
    sinh d       = Data.Number.MPFR.Base.sinh Up (getPrec d) d
    cosh d       = Data.Number.MPFR.Base.cosh Up (getPrec d) d
    tanh d       = Data.Number.MPFR.Base.tanh Up (getPrec d) d
    asinh d      = Data.Number.MPFR.Base.asinh Up (getPrec d) d
    acosh d      = Data.Number.MPFR.Base.acosh Up (getPrec d) d
    atanh d      = Data.Number.MPFR.Base.atanh Up (getPrec d) d

instance RealFrac MPFR where
    properFraction d = (fromIntegral n, f)
        where r = toRational d
              m = numerator r
              e = denominator r
              n = quot m e
              f = frac Up (getPrec d) d
