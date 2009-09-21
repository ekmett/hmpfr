{-# LANGUAGE MagicHash, CPP #-}
{-|
    Module      :  Data.Number.MPFR.Zero
    Description :  top level
    Copyright   :  (c) Aleš Bizjak
    License     :  BSD3

    Maintainer  :  ales.bizjak0@gmail.com
    Stability   :  experimental
    Portability :  non-portable

  This module defines instances 'Num', 'Real', 'Fractional', 'Floating' and 'RealFrac' of 'MPFR'.
  Operations are rounded with 'RoundMode' 'Zero' and computed with maximum precision of two 
  operands or with the precision of the operand. Otherwise it is equivalent to 
  "Data.Number.MPFR"
-}

{-# INCLUDE <mpfr.h> #-}
{-# INCLUDE <chsmpfr.h> #-}


module Data.Number.MPFR.Zero (
         -- * Assignment functions
         -- | See <http://www.mpfr.org/mpfr-current/mpfr.html#Assignment-Functions>
         --  documentation on particular functions.
         module Data.Number.MPFR.Assignment,
         -- * Conversion functions
         -- |  See <http://www.mpfr.org/mpfr-current/mpfr.html#Conversion-Functions>
         --  documentation on particular functions.
         module Data.Number.MPFR.Conversion,
         -- * Basic arithmetic functions
         -- |  For documentation on particular functions see
         -- <http://www.mpfr.org/mpfr-current/mpfr.html#Basic-Arithmetic-Functions>.
         module Data.Number.MPFR.Arithmetic,
         -- * Comparison functions
         -- | For documentation on particular functions see
         -- <http://www.mpfr.org/mpfr-current/mpfr.html#Comparison-Functions>
         module Data.Number.MPFR.Comparison,
         -- * Special functions
         -- | For documentation on particular functions see
         -- <http://www.mpfr.org/mpfr-current/mpfr.html#Special-Functions>.

         module Data.Number.MPFR.Special,
         -- * Integer related functions
         -- | For documentation on particular functions see
         -- <http://www.mpfr.org/mpfr-chttp://www.mpfr.org/mpfr-current/mpfr.html#Integer-Related-Functions>
         module Data.Number.MPFR.Integer,
         -- * Miscellaneous functions
         -- |For documentation on particular functions see
         -- <http://www.mpfr.org/mpfr-current/mpfr.html#Miscellaneous-Functions>.
         module Data.Number.MPFR.Misc, 
         RoundMode (Near, Up, Down, Zero),
         MPFR, Precision(), Exp, MpSize
)
where

import Data.Number.MPFR.Assignment 
import Data.Number.MPFR.Conversion
import qualified Data.Number.MPFR.Arithmetic as A
import Data.Number.MPFR.Arithmetic
import Data.Number.MPFR.Comparison
import qualified Data.Number.MPFR.Special as S
import Data.Number.MPFR.Special 
import Data.Number.MPFR.Integer
import Data.Number.MPFR.Misc

import Data.Number.MPFR.Internal

import Data.Maybe

import Data.Ratio

#if __GLASGOW_HASKELL__ >= 610
import GHC.Integer.Internals
#endif
import GHC.Exts

instance Num MPFR where
    d + d'        = add Zero (maxPrec d d') d d'
    d - d'        = sub Zero (maxPrec d d') d d'
    d * d'        = mul Zero (maxPrec d d') d d'
    negate d      = neg Zero (getPrec d) d
    abs d         = absD Zero (getPrec d) d
    signum        = fromInt Zero minPrec . fromMaybe (-1) . sgn
    fromInteger (S# i) = fromInt Zero minPrec (I# i)
    fromInteger i@(J# n _) = fromIntegerA Zero (fromIntegral . abs $ I# n * bitsPerIntegerLimb) i 

instance Real MPFR where
    toRational d = n % 2 ^ e
        where (n', e') = decompose d
              (n, e) = if e' >= 0 then ((n' * 2 ^ e'), 0)
                         else (n', - e')

instance Fractional MPFR where
    d / d'         = A.div Zero (maxPrec d d') d d'
    fromRational r = fromInteger n / fromInteger d
        where n = numerator r
              d = denominator r
    recip d        = one / d

instance Floating MPFR where
    pi           = S.pi Zero 53
    exp d        = S.exp Zero (getPrec d) d
    log d        = S.log Zero (getPrec d) d
    sqrt d       = A.sqrt Zero (getPrec d) d 
    (**) d d'    = A.pow Zero (maxPrec d d') d d'
    logBase d d' = Prelude.log d' / Prelude.log d
    sin d        = S.sin Zero (getPrec d) d
    cos d        = S.cos Zero (getPrec d) d
    tan d        = S.tan Zero (getPrec d) d
    asin d       = S.asin Zero (getPrec d) d
    acos d       = S.acos Zero (getPrec d) d
    atan d       = S.atan Zero (getPrec d) d
    sinh d       = S.sinh Zero (getPrec d) d
    cosh d       = S.cosh Zero (getPrec d) d
    tanh d       = S.tanh Zero (getPrec d) d
    asinh d      = S.asinh Zero (getPrec d) d
    acosh d      = S.acosh Zero (getPrec d) d
    atanh d      = S.atanh Zero (getPrec d) d

instance RealFrac MPFR where
    properFraction d = (fromIntegral n, f)
        where r = toRational d
              m = numerator r
              e = denominator r
              n = quot m e
              f = frac Zero (getPrec d) d
