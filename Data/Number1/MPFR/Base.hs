{-# INCLUDE <mpfr.h> #-}
{-# INCLUDE <chsmpfr.h> #-}

module Data.Number1.MPFR.Base (
         module Data.Number1.MPFR.Assignment,
         module Data.Number1.MPFR.Conversion,
         module Data.Number1.MPFR.Arithmetic,
         module Data.Number1.MPFR.Comparison,
         module Data.Number1.MPFR.Special,
         module Data.Number1.MPFR.Integer,
         module Data.Number1.MPFR.Misc,
         RoundMode (Near, Up, Down, Zero),
         Dyadic
                              )
where

import Data.Number1.MPFR.Assignment
import Data.Number1.MPFR.Conversion
import Data.Number1.MPFR.Arithmetic
import Data.Number1.MPFR.Comparison
import Data.Number1.MPFR.Special
import Data.Number1.MPFR.Integer
import Data.Number1.MPFR.Misc

import Data.Number1.MPFR.Internal

import Data.Maybe

instance Eq Dyadic where
    (==) = equal

instance Ord Dyadic where
    compare d d' = fromMaybe GT (cmp d d')
    (<)          = less
    (<=)         = lesseq
    (>)          = greater
    (>=)         = greatereq
    max d d'     = maxD Zero (maxPrec d d') d d'
    min d d'     = minD Zero (maxPrec d d') d d'
                    
instance Show Dyadic where
    show = toStringExp 16
