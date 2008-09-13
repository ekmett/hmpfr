{-# INCLUDE <mpfr.h> #-}
{-# INCLUDE <chsmpfr.h> #-}

module Data.Number1.MPFR.Base where

import Data.Number1.MPFR.Arithmetic

import Data.Number1.MPFR.Misc

import Data.Number1.MPFR.Internal

import Data.Number1.MPFR.Special as S

instance Num Dyadic where
    d + d'        = add Near (maxPrec d d') d d'
    d - d'        = sub Near (maxPrec d d') d d'
    d * d'        = mul Near (maxPrec d d') d d'
    negate d      = neg Near (getPrec d) d
    abs d         = absD Near (getPrec d) d
    signum d      = fromInt Near minPrec (fromMaybe (-1) (sgn d))
    fromInteger i = fromIntegerA Zero (checkPrec $ binprec i) i
                    -- todo this isn't totally correct

instance Real Dyadic where
    toRational d = n % 2 ^ e
        where (n', e') = decompose d
              (n, e) = if e' >= 0 then ((n' * 2 ^ e'), 0)
                         else (n', - e')

instance Fractional Dyadic where
    d / d'               = div Up (maxPrec d d') d d'
    fromRational (n % d) = (fromInteger n) / (fromInteger d)
    recip d              = one / d

instance Floating Dyadic where
    pi           = S.pi Near 53
    exp d        = S.exp Near (getPrec d) d
    log d        = S.log Near (getPrec d) d
    sqrt d       = S.sqrt Near (getPrec d) d 
    (**) d d'    = S.pow Near (maxPrec d d') d d'
    logBase d d' = S.log Near (getPrec d) d / S.log Near (getPrec d') d'
    sin d        = S.sin Near (getPrec d) d
    cos d        = S.cos Near (getPrec d) d
    tan d        = S.tan Near (getPrec d) d
    asin d       = S.asin Near (getPrec d) d
    acos d       = S.acos Near (getPrec d) d
    atan d       = S.atan Near (getPrec d) d
    sinh d       = S.sinh Near (getPrec d) d
    cosh d       = S.cosh Near (getPrec d) d
    tanh d       = S.tanh Near (getPrec d) d
    asinh d      = S.asinh Near (getPrec d) d
    acosh d      = S.acosh Near (getPrec d) d
    atanh d      = S.atanh Near (getPrec d) d

instance RealFrac Dyadic where
    properFraction d = (fromIntegral n, f)
        where m % e = toRational d
              n = quot m e
              f = frac Near (getPrec d) d
