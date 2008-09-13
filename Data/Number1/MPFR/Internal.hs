{-# INCLUDE <mpfr.h> #-}
{-# INCLUDE <chsmpfr.h> #-}

module Data.Number1.MPFR.Internal (
       module FFIhelper, 
       withDyadicsBA, withDyadicBAui, withDyadicBAiu, withDyadicBAsi, withDyadicBAis, 
       withDyadicB, withDyadicP, withDyadic, withDyadicBB, withDyadicC, 
       withDyadicF, withDyadicUI, withDyadicR, checkPrec, getMantissa', binprec,
       unsafePerformIO, peek, Ptr, nullPtr, mallocForeignPtrBytes, with,
       withForeignPtr, CInt, CLong, CULong, withCString, peekCString, alloca,
       peekArray, shiftL, Word, minPrec,
       
       Dyadic, Precision
)
where

import Data.Number1.MPFR.FFIhelper as FFIhelper

import Foreign.C(CInt, CLong, CULong, withCString, peekCString)
import Foreign.Marshal(alloca, peekArray)
import Foreign(unsafePerformIO, peek, Ptr, nullPtr, mallocForeignPtrBytes, with, withForeignPtr)

import Data.Bits(shiftL)

import Data.Word(Word)
import Prelude hiding (div, sqrt, read, isNaN, isInfinite, exp, log, sinh, cosh, tanh, pi)

type Dyadic = MPFR_T

type Precision = Word


-- these are helper functions, only for internal use
{-# INLINE withDyadicsBA #-}
withDyadicsBA               :: RoundMode -> Precision -> Dyadic -> Dyadic
                               -> (Ptr MPFR_T -> Ptr MPFR_T -> Ptr MPFR_T -> CRoundMode -> IO CInt)
                               -> (Dyadic, Int)
withDyadicsBA r p mp1 mp2 f = unsafePerformIO go
    where go = do ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  let dummy = MP (fromIntegral p) 0 0 fp
                  with dummy $ \p1 -> do
                    with mp1 $ \p2 -> do
                      with mp2 $ \p3 -> do
                          r2 <- f p1 p2 p3 ((fromIntegral . fromEnum) r)
                          r1 <- peekP p1 fp
                          return (r1, fromIntegral r2)

{-# INLINE withDyadicBAui #-}
withDyadicBAui :: RoundMode -> Precision -> Dyadic -> CULong
                  ->  (Ptr MPFR_T -> Ptr MPFR_T -> CULong -> CRoundMode -> IO CInt)
                  -> (Dyadic, Int) 
withDyadicBAui r p mp1 d f = unsafePerformIO go
    where go = do ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  let dummy = MP (fromIntegral p) 0 0 fp
                  with dummy $ \p1 -> do
                    with mp1 $ \p2 -> do
                      r2 <- f p1 p2 d ((fromIntegral . fromEnum) r)
                      r1 <- peekP p1 fp
                      return (r1, fromIntegral r2)
                                
{-# INLINE withDyadicBAsi #-}
withDyadicBAsi             :: RoundMode -> Precision -> Dyadic -> CLong
                              -> (Ptr MPFR_T -> Ptr MPFR_T -> CLong -> CRoundMode -> IO CInt)
                              -> (Dyadic, Int)
withDyadicBAsi r p mp1 d f = unsafePerformIO go 
    where go = do ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  let dummy = MP (fromIntegral p) 0 0 fp
                  with dummy $ \p1 -> do
                    with mp1 $ \p2 -> do
                      r2 <- f p1 p2 d ((fromIntegral . fromEnum) r)
                      r1 <- peekP p1 fp
                      return (r1, fromIntegral r2)
                                  
{-# INLINE withDyadicBAiu #-}
withDyadicBAiu             :: RoundMode -> Precision -> CULong -> Dyadic
                              -> (Ptr MPFR_T -> CULong -> Ptr MPFR_T -> CRoundMode -> IO CInt)
                              -> (Dyadic, Int) 
withDyadicBAiu r p d mp1 f = unsafePerformIO go 
    where go = do ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  let dummy = MP (fromIntegral p) 0 0 fp
                  with dummy $ \p1 -> do
                    with mp1 $ \p2 -> do
                      r2 <- f p1 d p2 ((fromIntegral . fromEnum) r)
                      r1 <- peekP p1 fp
                      return (r1, fromIntegral r2)

{-# INLINE withDyadicBAis #-}
withDyadicBAis             :: RoundMode -> Precision -> CLong -> Dyadic
                              -> (Ptr MPFR_T -> CLong -> Ptr MPFR_T -> CRoundMode -> IO CInt)
                              -> (Dyadic, Int) 
withDyadicBAis r p d mp1 f = unsafePerformIO go
    where go = do ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  let dummy = MP (fromIntegral p) 0 0 fp
                  with dummy $ \p1 -> do
                    with mp1 $ \p2 -> do
                      r2 <- f p1 d p2 ((fromIntegral . fromEnum) r)
                      r1 <- peekP p1 fp
                      return (r1, fromIntegral r2)

{-# INLINE withDyadicB #-}
withDyadicB       :: Dyadic -> (Ptr MPFR_T -> IO CInt) -> CInt 
withDyadicB mp1 f = unsafePerformIO go
    where go = with mp1 $ \p1 -> f p1

withDyadicP       :: Dyadic -> (Ptr MPFR_T -> IO CPrecision) -> CPrecision 
withDyadicP mp1 f = unsafePerformIO go
    where go = with mp1 $ \p1 -> f p1

{-# INLINE withDyadic #-}
withDyadic           :: RoundMode -> Precision -> Dyadic 
                        -> (Ptr MPFR_T -> Ptr MPFR_T -> CRoundMode -> IO CInt) 
                        -> (Dyadic, Int)
withDyadic r p mp1 f = unsafePerformIO go 
    where go = do ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  let dummy = MP (fromIntegral p) 0 0 fp
                  with dummy $ \p1 -> do
                    with mp1 $ \p2 -> do
                      r2 <- f p1 p2 ((fromIntegral . fromEnum) r)
                      r1 <- peekP p1 fp
                      return (r1, fromIntegral r2)
                  
{-# INLINE withDyadicBB #-}
withDyadicBB           :: Dyadic -> Dyadic 
                          -> (Ptr MPFR_T -> Ptr MPFR_T -> IO CInt) 
                          -> CInt  
withDyadicBB mp1 mp2 f = unsafePerformIO go
    where go = do with mp1 $ \p1 -> do 
                    with mp2 $ \p2 -> do 
                                      f p1 p2
                              
{-# INLINE withDyadicC #-}
withDyadicC       :: RoundMode -> Precision ->
                     (Ptr MPFR_T -> CRoundMode -> IO CInt) -> (Dyadic, Int)
withDyadicC r p f = unsafePerformIO go
    where go = do ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  let dummy = MP (fromIntegral p) 0 0 fp
                  with dummy $ \p1 -> do
                    r2 <- f p1 ((fromIntegral . fromEnum) r)
                    r1 <- peekP p1 fp
                    return (r1, fromIntegral r2)
   
withDyadicF         :: Dyadic -> RoundMode
                       -> (Ptr MPFR_T -> CRoundMode -> IO CInt)
                       -> Int
withDyadicF mp1 r f = (fromIntegral . unsafePerformIO) go
    where go = do with mp1 $ \p1 -> f p1 ((fromIntegral . fromEnum) r)

withDyadicUI         :: RoundMode -> Precision -> Word
                        -> (Ptr MPFR_T -> CULong -> CRoundMode -> IO CInt)
                        -> (Dyadic, Int)
withDyadicUI r p d f = unsafePerformIO go 
    where go = do ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  let dummy = MP (fromIntegral p) 0 0 fp
                  with dummy $ \p1 -> do
                    r2 <- f p1 (fromIntegral d) ((fromIntegral . fromEnum) r)
                    r1 <- peekP p1 fp
                    return (r1, fromIntegral r2)

withDyadicR       :: Precision -> Dyadic
                     -> (Ptr MPFR_T -> Ptr MPFR_T -> IO CInt)
                     -> (Dyadic, Int)
withDyadicR p d f = unsafePerformIO go
    where go = do ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  let dummy = MP (fromIntegral p) 0 0 fp
                  with dummy $ \p1 -> do
                    with d $ \p2 -> do
                      r2 <- f p1 p2
                      r1 <- peekP p1 fp
                      return (r1, fromIntegral r2)
                        

checkPrec :: Precision -> Precision
checkPrec = max minPrec

getMantissa'     :: Dyadic -> [Limb]
getMantissa' (MP p _ _ p1) = unsafePerformIO go
    where go = do withForeignPtr p1 $ \pt -> do 
                    arr <- peekArray (Prelude.ceiling ((fromIntegral p ::Double) / fromIntegral bitsPerMPLimb)) pt ;
                    return arr 

{- TODO: this is inefficient 
binprec   :: Integer -> Precision
binprec i = length (takeWhile (/= 0) (iterate (flip shiftR 1) i)
-}
-- TODO
binprec   :: Integer -> Precision
binprec d = Prelude.floor (logBase 2 (fromInteger (if d >= 0 then d else -d)) :: Double) + 1

--one ::  Dyadic              
--one = fromWord Near minPrec 1

--zero :: Dyadic              
--ggzero = fromWord Near minPrec 0

minPrec :: Precision
minPrec = 32