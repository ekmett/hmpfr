{-# INCLUDE <mpfr.h> #-}
{-# INCLUDE <chsmpfr.h> #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Number1.MPFR.Down (
       module B
)
where

import Data.Number1.MPFR.Base as B

import Data.Number1.MPFR.Internal

import Data.Maybe

import Data.Ratio

instance Num MPFR where
    d + d'        = add Down (maxPrec d d') d d'
    d - d'        = sub Down (maxPrec d d') d d'
    d * d'        = mul Down (maxPrec d d') d d'
    negate d      = neg Down (getPrec d) d
    abs d         = absD Down (getPrec d) d
    signum d      = fromInt Down minPrec (fromMaybe (-1) (sgn d))
    fromInteger i = fromIntegerA Zero (checkPrec $ binprec i) i
                    -- TODO works only partially

instance Real MPFR where
    toRational d = n % 2 ^ e
        where (n', e') = decompose d
              (n, e) = if e' >= 0 then ((n' * 2 ^ e'), 0)
                         else (n', - e')

instance Fractional MPFR where
    d / d'         = B.div Down (maxPrec d d') d d'
    fromRational r = (fromInteger n) / (fromInteger d)
        where n = numerator r
              d = denominator r
    recip d        = one / d

instance Floating MPFR where
    pi           = B.pi Down 53
    exp d        = B.exp Down (getPrec d) d
    log d        = B.log Down (getPrec d) d
    sqrt d       = B.sqrt Down (getPrec d) d 
    (**) d d'    = B.pow Down (maxPrec d d') d d'
    logBase d d' = Prelude.log d / Prelude.log d'
    sin d        = B.sin Down (getPrec d) d
    cos d        = B.cos Down (getPrec d) d
    tan d        = B.tan Down (getPrec d) d
    asin d       = B.asin Down (getPrec d) d
    acos d       = B.acos Down (getPrec d) d
    atan d       = B.atan Down (getPrec d) d
    sinh d       = B.sinh Down (getPrec d) d
    cosh d       = B.cosh Down (getPrec d) d
    tanh d       = B.tanh Down (getPrec d) d
    asinh d      = B.asinh Down (getPrec d) d
    acosh d      = B.acosh Down (getPrec d) d
    atanh d      = B.atanh Down (getPrec d) d

instance RealFrac MPFR where
    properFraction d = (fromIntegral n, f)
        where r = toRational d
              m = numerator r
              e = denominator r
              n = quot m e
              f = frac Down (getPrec d) d
