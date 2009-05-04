{-|
    Module      :  Data.Number.MPFR.Integer
    Description :  wrappers for integer related functions
    Copyright   :  (c) Aleš Bizjak
    License     :  BSD3

    Maintainer  :  ales.bizjak0@gmail.com
    Stability   :  experimental
    Portability :  non-portable

 Integer related functions. See MPFR manual for detailed documentation.
-}
{-# INCLUDE <mpfr.h> #-}
{-# INCLUDE <chsmpfr.h> #-}


module Data.Number.MPFR.Integer where

import Data.Number.MPFR.Internal

rint     :: RoundMode -> Precision -> MPFR -> MPFR
rint r p = fst . rint_ r p

ceil   :: Precision -> MPFR -> MPFR
ceil p = fst . ceil_ p

floor   :: Precision -> MPFR -> MPFR
floor p = fst . floor_ p

round   :: Precision -> MPFR -> MPFR
round p = fst . round_ p

trunc   :: Precision -> MPFR -> MPFR
trunc p = fst . trunc_ p

rintCeil     :: RoundMode -> Precision -> MPFR -> MPFR
rintCeil r p = fst . rintCeil_ r p

rintFloor     :: RoundMode -> Precision -> MPFR -> MPFR
rintFloor r p = fst . rintFloor_ r p

rintRound     :: RoundMode -> Precision -> MPFR -> MPFR
rintRound r p = fst . rintRound_ r p

rintTrunc     :: RoundMode -> Precision -> MPFR -> MPFR
rintTrunc r p = fst . rintTrunc_ r p

frac     :: RoundMode -> Precision -> MPFR -> MPFR
frac r p = fst . frac_ r p

remainder       :: RoundMode -> Precision -> MPFR -> MPFR -> MPFR
remainder r p d = fst . remainder_ r p d

remquo          :: RoundMode -> Precision -> MPFR -> MPFR -> (MPFR, Int)
remquo r p d d' = case remquo_ r p d d' of
                     (a, b, _) -> (a, b)

rint_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
rint_ r p d = withMPFR r p d mpfr_rint

ceil_     :: Precision -> MPFR -> (MPFR, Int)
ceil_ p d = withMPFRR p d mpfr_ceil

floor_     :: Precision -> MPFR -> (MPFR, Int)
floor_ p d = withMPFRR p d mpfr_floor

round_     :: Precision -> MPFR -> (MPFR, Int)
round_ p d = withMPFRR p d mpfr_round

trunc_     :: Precision -> MPFR -> (MPFR, Int)
trunc_ p d = withMPFRR p d mpfr_trunc

rintCeil_ :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
rintCeil_ r p d = withMPFR r p d mpfr_rint_ceil

rintFloor_ :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
rintFloor_ r p d = withMPFR r p d mpfr_rint_floor

rintRound_ :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
rintRound_ r p d = withMPFR r p d mpfr_rint_round

rintTrunc_ :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
rintTrunc_ r p d = withMPFR r p d mpfr_rint_trunc

frac_ :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
frac_ r p d = withMPFR r p d mpfr_frac

remainder_          :: RoundMode -> Precision -> MPFR -> MPFR -> (MPFR,Int)
remainder_ r p d d' = withMPFRsBA r p d d' mpfr_remainder

remquo_          :: RoundMode -> Precision -> MPFR -> MPFR -> (MPFR, Int, Int)
remquo_ r p d d' = unsafePerformIO go
    where go = do ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  alloca $ \p1 -> do
                    pokeDummy p1 fp p
                    with d $ \p2 -> 
                      with d' $ \p3 -> 
                        alloca $ \p4 -> do
                          r3 <- mpfr_remquo p1 p4 p2 p3 ((fromIntegral . fromEnum) r)
                          r1 <- peekP p1 fp
                          r2 <- peek p4
                          return (r1, fromIntegral r2, fromIntegral r3)

isInteger   :: MPFR -> Bool
isInteger d = withMPFRB d mpfr_integer_p /= 0
