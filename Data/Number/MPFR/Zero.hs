{-|
    Module      :  Data.Number.MPFR.Zero
    Description :  top level
    Copyright   :  (c) Aleš Bizjak
    License     :  BSD3

    Maintainer  :  ales.bizjak0@gmail.com
    Stability   :  experimental
    Portability :  portable

 This module defines instances Num, Real, Fractional, Floating and RealFrac of MPFR.
 Operations are rounded with RoundMode Zero and computed with max precision of two 
 operands or with the precision of the operand. Otherwise it is equivalent to 
 Data.Number.MPFR

-}

{-# INCLUDE <mpfr.h> #-}
{-# INCLUDE <chsmpfr.h> #-}

module Data.Number.MPFR.Zero (
       module Data.Number.MPFR.Base 
)
where

import Data.Number.MPFR.Base 

import Data.Number.MPFR.Internal

import Data.Maybe

import Data.Ratio

instance Num MPFR where
    d + d'        = add Zero (maxPrec d d') d d'
    d - d'        = sub Zero (maxPrec d d') d d'
    d * d'        = mul Zero (maxPrec d d') d d'
    negate d      = neg Zero (getPrec d) d
    abs d         = absD Zero (getPrec d) d
    signum d      = fromInt Zero minPrec (fromMaybe (-1) (sgn d))
    fromInteger i = fromIntegerA Zero (checkPrec $ binprec i) i
                    -- TODO works only partially

instance Real MPFR where
    toRational d = n % 2 ^ e
        where (n', e') = decompose d
              (n, e) = if e' >= 0 then ((n' * 2 ^ e'), 0)
                         else (n', - e')

instance Fractional MPFR where
    d / d'         = Data.Number.MPFR.Base.div Zero (maxPrec d d') d d'
    fromRational r = (fromInteger n) / (fromInteger d)
        where n = numerator r
              d = denominator r
    recip d        = one / d

instance Floating MPFR where
    pi           = Data.Number.MPFR.Base.pi Zero 53
    exp d        = Data.Number.MPFR.Base.exp Zero (getPrec d) d
    log d        = Data.Number.MPFR.Base.log Zero (getPrec d) d
    sqrt d       = Data.Number.MPFR.Base.sqrt Zero (getPrec d) d 
    (**) d d'    = Data.Number.MPFR.Base.pow Zero (maxPrec d d') d d'
    logBase d d' = Prelude.log d' / Prelude.log d
    sin d        = Data.Number.MPFR.Base.sin Zero (getPrec d) d
    cos d        = Data.Number.MPFR.Base.cos Zero (getPrec d) d
    tan d        = Data.Number.MPFR.Base.tan Zero (getPrec d) d
    asin d       = Data.Number.MPFR.Base.asin Zero (getPrec d) d
    acos d       = Data.Number.MPFR.Base.acos Zero (getPrec d) d
    atan d       = Data.Number.MPFR.Base.atan Zero (getPrec d) d
    sinh d       = Data.Number.MPFR.Base.sinh Zero (getPrec d) d
    cosh d       = Data.Number.MPFR.Base.cosh Zero (getPrec d) d
    tanh d       = Data.Number.MPFR.Base.tanh Zero (getPrec d) d
    asinh d      = Data.Number.MPFR.Base.asinh Zero (getPrec d) d
    acosh d      = Data.Number.MPFR.Base.acosh Zero (getPrec d) d
    atanh d      = Data.Number.MPFR.Base.atanh Zero (getPrec d) d

instance RealFrac MPFR where
    properFraction d = (fromIntegral n, f)
        where r = toRational d
              m = numerator r
              e = denominator r
              n = quot m e
              f = frac Zero (getPrec d) d
