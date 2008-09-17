{-# INCLUDE <mpfr.h> #-}
{-# INCLUDE <chsmpfr.h> #-}

-- | This module defines instances Num, Real, Fractional, Floating and RealFrac of MPFR.
-- Operations are rounded with RoundMode Near and computed with max precision of two 
-- operands or with the precision of the operand. Otherwise it is equivalent to 
-- Data.Number.MPFR

module Data.Number1.MPFR.Near (
       module Data.Number1.MPFR.Base
)
where

import Data.Number1.MPFR.Base

import Data.Number1.MPFR.Internal

import Data.Maybe

import Data.Ratio

instance Num MPFR where
    d + d'        = add Near (maxPrec d d') d d'
    d - d'        = sub Near (maxPrec d d') d d'
    d * d'        = mul Near (maxPrec d d') d d'
    negate d      = neg Near (getPrec d) d
    abs d         = absD Near (getPrec d) d
    signum d      = fromInt Near minPrec (fromMaybe (-1) (sgn d))
    fromInteger i = fromIntegerA Zero (checkPrec $ binprec i) i
                    -- TODO works only partially

instance Real MPFR where
    toRational d = n % 2 ^ e
        where (n', e') = decompose d
              (n, e) = if e' >= 0 then ((n' * 2 ^ e'), 0)
                         else (n', - e')

instance Fractional MPFR where
    d / d'         = Data.Number1.MPFR.Base.div Up (maxPrec d d') d d'
    fromRational r = (fromInteger n) / (fromInteger d)
        where n = numerator r
              d = denominator r
    recip d        = one / d

instance Floating MPFR where
    pi           = Data.Number1.MPFR.Base.pi Near 53
    exp d        = Data.Number1.MPFR.Base.exp Near (getPrec d) d
    log d        = Data.Number1.MPFR.Base.log Near (getPrec d) d
    sqrt d       = Data.Number1.MPFR.Base.sqrt Near (getPrec d) d 
    (**) d d'    = Data.Number1.MPFR.Base.pow Near (maxPrec d d') d d'
    logBase d d' = Prelude.log d' / Prelude.log d
    sin d        = Data.Number1.MPFR.Base.sin Near (getPrec d) d
    cos d        = Data.Number1.MPFR.Base.cos Near (getPrec d) d
    tan d        = Data.Number1.MPFR.Base.tan Near (getPrec d) d
    asin d       = Data.Number1.MPFR.Base.asin Near (getPrec d) d
    acos d       = Data.Number1.MPFR.Base.acos Near (getPrec d) d
    atan d       = Data.Number1.MPFR.Base.atan Near (getPrec d) d
    sinh d       = Data.Number1.MPFR.Base.sinh Near (getPrec d) d
    cosh d       = Data.Number1.MPFR.Base.cosh Near (getPrec d) d
    tanh d       = Data.Number1.MPFR.Base.tanh Near (getPrec d) d
    asinh d      = Data.Number1.MPFR.Base.asinh Near (getPrec d) d
    acosh d      = Data.Number1.MPFR.Base.acosh Near (getPrec d) d
    atanh d      = Data.Number1.MPFR.Base.atanh Near (getPrec d) d

instance RealFrac MPFR where
    properFraction d = (fromIntegral n, f)
        where r = toRational d
              m = numerator r
              e = denominator r
              n = quot m e
              f = frac Near (getPrec d) d
