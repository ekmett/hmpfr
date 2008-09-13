{-# INCLUDE <mpfr.h> #-}
{-# INCLUDE <chsmpfr.h> #-}

module Data.Number1.MPFR.Base where

import Data.Number1.MPFR.Comparison

import Data.Number1.MPFR.Misc

import Data.Number1.MPFR.Internal

import Data.Number1.MPFR.Special

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
                    