{-# INCLUDE <mpfr.h> #-}
{-# INCLUDE <chsmpfr.h> #-}

module Data.Number1.MPFR.Misc where

import Data.Number1.MPFR.Internal

import Data.Number1.MPFR.Assignment

import Data.Number1.MPFR.Comparison

nextToward         :: Dyadic -> Dyadic -> Dyadic
nextToward mp1 mp2 = unsafePerformIO go
    where go = do let p = fromIntegral (getPrec mp1)
                  ls <- mpfr_custom_get_size p
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  let dummy = MP p 0 0 fp
                  with dummy $ \p1 -> do
                      with mp1 $ \p2 -> do 
                        with mp2 $ \p3 -> do
                          _ <- mpfr_set p1 p2 ((fromIntegral . fromEnum) Near) 
                          mpfr_nexttoward p1 p3 
                          peekP p1 fp


nextAbove     :: Dyadic -> Dyadic
nextAbove mp1 = unsafePerformIO go
    where go = do let p = fromIntegral (getPrec mp1)
                  ls <- mpfr_custom_get_size p
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  let dummy = MP p 0 0 fp
                  with dummy $ \p1 -> do
                      with mp1 $ \p2 -> do 
                        _ <- mpfr_set p1 p2 ((fromIntegral . fromEnum) Near) 
                        mpfr_nextabove p1 
                        peekP p1 fp

nextBelow     :: Dyadic -> Dyadic
nextBelow mp1 = unsafePerformIO go
    where go = do let p = fromIntegral (getPrec mp1)
                  ls <- mpfr_custom_get_size p
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  let dummy = MP p 0 0 fp
                  with dummy $ \p1 -> do
                      with mp1 $ \p2 -> do 
                        _ <- mpfr_set p1 p2 ((fromIntegral . fromEnum) Near) 
                        mpfr_nextbelow p1 
                        peekP p1 fp

maxD           :: RoundMode -> Precision -> Dyadic -> Dyadic -> Dyadic
maxD r p d1 d2 = fst $ maxD_ r p d1 d2

minD           :: RoundMode -> Precision -> Dyadic -> Dyadic -> Dyadic
minD r p d1 d2 = fst $ minD_ r p d1 d2

random2       :: Precision -> MpSize -> Exp -> IO Dyadic
random2 p m e = do ls <- mpfr_custom_get_size (fromIntegral p)
                   fp <- mallocForeignPtrBytes (fromIntegral ls)
                   let dummy = MP (fromIntegral p) 0 0 fp
                   with dummy $ \p1 -> do
                     mpfr_random2 p1 m e
                     peekP p1 fp

getExp   :: Dyadic -> Exp
getExp d = (fromIntegral . unsafePerformIO) go
                 where go = do with d $ \p1 -> mpfr_custom_get_exp p1

setExp     :: Dyadic -> Exp -> Dyadic
setExp d e = unsafePerformIO go
    where go = do let p = fromIntegral (getPrec d)
                  ls <- mpfr_custom_get_size p
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  let dummy = MP p 0 0 fp
                  with dummy $ \p1 -> do
                    with d $ \p2 -> do 
                      mpfr_set p1 p2 ((fromIntegral . fromEnum) Near)
                      mpfr_set_exp p1 e
                      peekP p1 fp

signbit   :: Dyadic -> Bool
signbit d = withDyadicB d mpfr_signbit /= 0

maxD_           :: RoundMode -> Precision -> Dyadic -> Dyadic -> (Dyadic, Int)
maxD_ r p d1 d2 = withDyadicsBA r p d1 d2 mpfr_max

minD_           :: RoundMode -> Precision -> Dyadic -> Dyadic -> (Dyadic, Int)
minD_ r p d1 d2 = withDyadicsBA r p d1 d2 mpfr_min

getPrec   :: Dyadic -> Precision
getPrec d = fromIntegral (withDyadicP d mpfr_get_prec)

-- | getMantissa and getExp return values such that
--
-- > d = getMantissa d * 2^(getExp d - Prelude.ceiling ((getPrec d) / bitsPerMPLimb)* bitsPerMPLimb )
getMantissa   :: Dyadic -> Integer
getMantissa d = if less d zero then -h else h
               where (h, _) = foldl (\(a,b) c -> (a + (toInteger c) `shiftL` b, b + bitsPerMPLimb)) (0,0) (getMantissa' d) 

one :: Dyadic
one = fromWord Near minPrec 1

zero :: Dyadic
zero = fromWord Near minPrec 0

maxPrec      :: Dyadic -> Dyadic -> Precision
maxPrec d d' = max (getPrec d) (getPrec d')