{-# INCLUDE <mpfr.h> #-}
{-# INCLUDE <chsmpfr.h> #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Number1.MPFR.Near (
       module B
)
where

import Data.Number1.MPFR.Base as B

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
    d / d'         = B.div Up (maxPrec d d') d d'
    fromRational r = (fromInteger n) / (fromInteger d)
        where n = numerator r
              d = denominator r
    recip d        = one / d

instance Floating MPFR where
    pi           = B.pi Near 53
    exp d        = B.exp Near (getPrec d) d
    log d        = B.log Near (getPrec d) d
    sqrt d       = B.sqrt Near (getPrec d) d 
    (**) d d'    = B.pow Near (maxPrec d d') d d'
    logBase d d' = Prelude.log d' / Prelude.log d
    sin d        = B.sin Near (getPrec d) d
    cos d        = B.cos Near (getPrec d) d
    tan d        = B.tan Near (getPrec d) d
    asin d       = B.asin Near (getPrec d) d
    acos d       = B.acos Near (getPrec d) d
    atan d       = B.atan Near (getPrec d) d
    sinh d       = B.sinh Near (getPrec d) d
    cosh d       = B.cosh Near (getPrec d) d
    tanh d       = B.tanh Near (getPrec d) d
    asinh d      = B.asinh Near (getPrec d) d
    acosh d      = B.acosh Near (getPrec d) d
    atanh d      = B.atanh Near (getPrec d) d

instance RealFrac MPFR where
    properFraction d = (fromIntegral n, f)
        where r = toRational d
              m = numerator r
              e = denominator r
              n = quot m e
              f = frac Near (getPrec d) d
