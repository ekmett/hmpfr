{-# INCLUDE <mpfr.h> #-}
{-# INCLUDE <chsmpfr.h> #-}
-- | Functions that don't belong anywhere else. See MPFR manual for detailed documentation.
module Data.Number1.MPFR.Misc where

import Data.Number1.MPFR.Internal

import Data.Number1.MPFR.Assignment


nextToward         :: MPFR -> MPFR -> MPFR
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


nextAbove     :: MPFR -> MPFR
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

nextBelow     :: MPFR -> MPFR
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

maxD           :: RoundMode -> Precision -> MPFR -> MPFR -> MPFR
maxD r p d1 d2 = fst $ maxD_ r p d1 d2

minD           :: RoundMode -> Precision -> MPFR -> MPFR -> MPFR
minD r p d1 d2 = fst $ minD_ r p d1 d2

random2       :: Precision -> MpSize -> Exp -> IO MPFR
random2 p m e = do ls <- mpfr_custom_get_size (fromIntegral p)
                   fp <- mallocForeignPtrBytes (fromIntegral ls)
                   let dummy = MP (fromIntegral p) 0 0 fp
                   with dummy $ \p1 -> do
                     mpfr_random2 p1 m e
                     peekP p1 fp

getExp   :: MPFR -> Exp
getExp d = (fromIntegral . unsafePerformIO) go
                 where go = do with d $ \p1 -> mpfr_custom_get_exp p1

setExp     :: MPFR -> Exp -> MPFR
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

signbit   :: MPFR -> Bool
signbit d = withMPFRB d mpfr_signbit /= 0

maxD_           :: RoundMode -> Precision -> MPFR -> MPFR -> (MPFR, Int)
maxD_ r p d1 d2 = withMPFRsBA r p d1 d2 mpfr_max

minD_           :: RoundMode -> Precision -> MPFR -> MPFR -> (MPFR, Int)
minD_ r p d1 d2 = withMPFRsBA r p d1 d2 mpfr_min

getPrec   :: MPFR -> Precision
getPrec d = fromIntegral (withMPFRP d mpfr_get_prec)

-- | getMantissa and getExp return values such that
--
-- > d = getMantissa d * 2^(getExp d - Prelude.ceiling ((getPrec d) / bitsPerMPLimb)* bitsPerMPLimb )
getMantissa   :: MPFR -> Integer
getMantissa d = toInteger (sign d) * h
               where (h, _) = foldl (\(a,b) c -> (a + (toInteger c) `shiftL` b, b + bitsPerMPLimb)) (0,0) (getMantissa' d) 

one :: MPFR
one = fromWord Near minPrec 1

zero :: MPFR
zero = fromWord Near minPrec 0

maxPrec      :: MPFR -> MPFR -> Precision
maxPrec d d' = max (getPrec d) (getPrec d')