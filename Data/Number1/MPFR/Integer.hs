{-# INCLUDE <mpfr.h> #-}
{-# INCLUDE <chsmpfr.h #-}

module Data.Number1.MPFR.Integer where

import Data.Number1.MPFR.Internal

rint       :: RoundMode -> Precision -> Dyadic -> Dyadic
rint r p d = fst $ rint_ r p d

ceil     :: Precision -> Dyadic -> Dyadic
ceil p d = fst $ ceil_ p d

floor     :: Precision -> Dyadic -> Dyadic
floor p d = fst $ floor_ p d

round     :: Precision -> Dyadic -> Dyadic
round p d = fst $ round_ p d

trunc     :: Precision -> Dyadic -> Dyadic
trunc p d = fst $ trunc_ p d

rintCeil :: RoundMode -> Precision -> Dyadic -> Dyadic
rintCeil r p d = fst $ rintCeil_ r p d

rintFloor :: RoundMode -> Precision -> Dyadic -> Dyadic
rintFloor r p d = fst $ rintFloor_ r p d

rintRound :: RoundMode -> Precision -> Dyadic -> Dyadic
rintRound r p d = fst $ rintRound_ r p d

rintTrunc :: RoundMode -> Precision -> Dyadic -> Dyadic
rintTrunc r p d = fst $ rintTrunc_ r p d

frac :: RoundMode -> Precision -> Dyadic -> Dyadic
frac r p d = fst $ frac_ r p d

remainder          :: RoundMode -> Precision -> Dyadic -> Dyadic -> Dyadic
remainder r p d d' = fst $ remainder_ r p d d'

remquo          :: RoundMode -> Precision -> Dyadic -> Dyadic -> (Dyadic, Int)
remquo r p d d' = case remquo_ r p d d' of
                     (a, b, _) -> (a, b)

rint_       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
rint_ r p d = withDyadic r p d mpfr_rint

ceil_     :: Precision -> Dyadic -> (Dyadic, Int)
ceil_ p d = withDyadicR p d mpfr_ceil

floor_     :: Precision -> Dyadic -> (Dyadic, Int)
floor_ p d = withDyadicR p d mpfr_floor

round_     :: Precision -> Dyadic -> (Dyadic, Int)
round_ p d = withDyadicR p d mpfr_round

trunc_     :: Precision -> Dyadic -> (Dyadic, Int)
trunc_ p d = withDyadicR p d mpfr_trunc

rintCeil_ :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
rintCeil_ r p d = withDyadic r p d mpfr_rint_ceil

rintFloor_ :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
rintFloor_ r p d = withDyadic r p d mpfr_rint_floor

rintRound_ :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
rintRound_ r p d = withDyadic r p d mpfr_rint_round

rintTrunc_ :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
rintTrunc_ r p d = withDyadic r p d mpfr_rint_trunc

frac_ :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
frac_ r p d = withDyadic r p d mpfr_frac

remainder_          :: RoundMode -> Precision -> Dyadic -> Dyadic -> (Dyadic,Int)
remainder_ r p d d' = withDyadicsBA r p d d' mpfr_remainder

remquo_          :: RoundMode -> Precision -> Dyadic -> Dyadic -> (Dyadic, Int, Int)
remquo_ r p d d' = unsafePerformIO go
    where go = do ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  let dummy = MP (fromIntegral p) 0 0 fp
                  with dummy $ \p1 -> do 
                    with d $ \p2 -> do
                      with d' $ \p3 -> do
                        alloca $ \p4 -> do
                          r3 <- mpfr_remquo p1 p4 p2 p3 ((fromIntegral . fromEnum) r)
                          r1 <- peekP p1 fp
                          r2 <- peek p4
                          return (r1, fromIntegral r2, fromIntegral r3)

isInteger   :: Dyadic -> Bool
isInteger d = withDyadicB d mpfr_integer_p /= 0
