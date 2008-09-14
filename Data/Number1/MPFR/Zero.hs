{-# LANGUAGE TypeSynonymInstances #-}
{-# INCLUDE <mpfr.h> #-}
{-# INCLUDE <chsmpfr.h> #-}

module Data.Number1.MPFR.Zero (
       module B
)
where

import Data.Number1.MPFR.Base as B

import Data.Number1.MPFR.Internal

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
    d / d'         = B.div Zero (maxPrec d d') d d'
    fromRational r = (fromInteger n) / (fromInteger d)
        where n = numerator r
              d = denominator r
    recip d        = one / d

instance Floating MPFR where
    pi           = B.pi Zero 53
    exp d        = B.exp Zero (getPrec d) d
    log d        = B.log Zero (getPrec d) d
    sqrt d       = B.sqrt Zero (getPrec d) d 
    (**) d d'    = B.pow Zero (maxPrec d d') d d'
    logBase d d' = Prelude.log d / Prelude.log d'
    sin d        = B.sin Zero (getPrec d) d
    cos d        = B.cos Zero (getPrec d) d
    tan d        = B.tan Zero (getPrec d) d
    asin d       = B.asin Zero (getPrec d) d
    acos d       = B.acos Zero (getPrec d) d
    atan d       = B.atan Zero (getPrec d) d
    sinh d       = B.sinh Zero (getPrec d) d
    cosh d       = B.cosh Zero (getPrec d) d
    tanh d       = B.tanh Zero (getPrec d) d
    asinh d      = B.asinh Zero (getPrec d) d
    acosh d      = B.acosh Zero (getPrec d) d
    atanh d      = B.atanh Zero (getPrec d) d

instance RealFrac MPFR where
    properFraction d = (fromIntegral n, f)
        where r = toRational d
              m = numerator r
              e = denominator r
              n = quot m e
              f = frac Zero (getPrec d) d
