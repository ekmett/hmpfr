{-# LANGUAGE MagicHash, CPP #-}
{-|
    Module      :  Data.Number.MPFR.Instances.Near
    Description :  Instance declarations
    Copyright   :  (c) Aleš Bizjak
    License     :  BSD3

    Maintainer  :  ales.bizjak0@gmail.com
    Stability   :  experimental
    Portability :  non-portable

  This module defines instances 'Num', 'Real', 'Fractional', 'Floating' and 'RealFrac' of 'MPFR'.
  Operations are rounded with 'RoundMode' 'Near' and computed with maximum precision of two 
  operands or with the precision of the operand.
-}

{-# INCLUDE <mpfr.h> #-}
{-# INCLUDE <chsmpfr.h> #-}


module Data.Number.MPFR.Instances.Near ()
where

import qualified Data.Number.MPFR.Arithmetic as A
import qualified Data.Number.MPFR.Special as S
import Data.Number.MPFR.Misc
import Data.Number.MPFR.Assignment
import Data.Number.MPFR.Comparison
import Data.Number.MPFR.Internal
import Data.Number.MPFR.Conversion
import Data.Number.MPFR.Integer

import Data.Maybe

import Data.Ratio

#if (__GLASGOW_HASKELL__ >= 610) && (__GLASGOW_HASKELL__ < 612)
import GHC.Integer.Internals
#elif __GLASGOW_HASKELL__ >= 612
import GHC.Integer.GMP.Internals
#endif

import qualified GHC.Exts as E

instance Num MPFR where
    d + d'        = A.add Near (maxPrec d d') d d'
    d - d'        = A.sub Near (maxPrec d d') d d'
    d * d'        = A.mul Near (maxPrec d d') d d'
    negate d      = A.neg Near (getPrec d) d
    abs d         = A.absD Near (getPrec d) d
    signum        = fromInt Near minPrec . fromMaybe (-1) . sgn
    fromInteger (S# i) = fromInt Near minPrec (E.I# i)
    fromInteger i@(J# n _) = fromIntegerA Zero (fromIntegral . abs $ E.I# n * bitsPerIntegerLimb) i 

instance Real MPFR where
    toRational d = n % 2 ^ e
        where (n', e') = decompose d
              (n, e) = if e' >= 0 then ((n' * 2 ^ e'), 0)
                         else (n', - e')

instance Fractional MPFR where
    d / d'         = A.div Up (maxPrec d d') d d'
    fromRational r = fromInteger n / fromInteger d
        where n = numerator r
              d = denominator r
    recip d        = one / d

instance Floating MPFR where
    pi           = S.pi Near 53
    exp d        = S.exp Near (getPrec d) d
    log d        = S.log Near (getPrec d) d
    sqrt d       = A.sqrt Near (getPrec d) d 
    (**) d d'    = A.pow Near (maxPrec d d') d d'
    logBase d d' = Prelude.log d' / Prelude.log d
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

instance RealFrac MPFR where
    properFraction d = (fromIntegral n, f)
        where r = toRational d
              m = numerator r
              e = denominator r
              n = quot m e
              f = frac Near (getPrec d) d
