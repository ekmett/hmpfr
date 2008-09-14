{-# LANGUAGE TypeSynonymInstances #-}
{-# INCLUDE <mpfr.h> #-}
{-# INCLUDE <chsmpfr.h> #-}

module Data.Number1.MPFR.Up (
       module B
)
where

import Data.Number1.MPFR.Base as B

import Data.Number1.MPFR.Internal

import Data.Maybe

import Data.Ratio

instance Num MPFR where
    d + d'        = add Up (maxPrec d d') d d'
    d - d'        = sub Up (maxPrec d d') d d'
    d * d'        = mul Up (maxPrec d d') d d'
    negate d      = neg Up (getPrec d) d
    abs d         = absD Up (getPrec d) d
    signum d      = fromInt Up minPrec (fromMaybe (-1) (sgn d))
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
    pi           = B.pi Up 53
    exp d        = B.exp Up (getPrec d) d
    log d        = B.log Up (getPrec d) d
    sqrt d       = B.sqrt Up (getPrec d) d 
    (**) d d'    = B.pow Up (maxPrec d d') d d'
    logBase d d' = Prelude.log d / Prelude.log d'
    sin d        = B.sin Up (getPrec d) d
    cos d        = B.cos Up (getPrec d) d
    tan d        = B.tan Up (getPrec d) d
    asin d       = B.asin Up (getPrec d) d
    acos d       = B.acos Up (getPrec d) d
    atan d       = B.atan Up (getPrec d) d
    sinh d       = B.sinh Up (getPrec d) d
    cosh d       = B.cosh Up (getPrec d) d
    tanh d       = B.tanh Up (getPrec d) d
    asinh d      = B.asinh Up (getPrec d) d
    acosh d      = B.acosh Up (getPrec d) d
    atanh d      = B.atanh Up (getPrec d) d

instance RealFrac MPFR where
    properFraction d = (fromIntegral n, f)
        where r = toRational d
              m = numerator r
              e = denominator r
              n = quot m e
              f = frac Up (getPrec d) d
