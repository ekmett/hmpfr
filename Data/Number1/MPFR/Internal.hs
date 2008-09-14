{-# INCLUDE <mpfr.h> #-}
{-# INCLUDE <chsmpfr.h> #-}

module Data.Number1.MPFR.Internal (
       module FFIhelper, 
       withMPFRsBA, withMPFRBAui, withMPFRBAiu, withMPFRBAsi, withMPFRBAis, 
       withMPFRB, withMPFRP, withMPFR, withMPFRBB, withMPFRC, 
       withMPFRF, withMPFRUI, withMPFRR, checkPrec, getMantissa', binprec,
       unsafePerformIO, peek, Ptr, nullPtr, mallocForeignPtrBytes, with,
       withForeignPtr, CInt, CLong, CULong, withCString, peekCString, alloca,
       peekArray, shiftL, Word, minPrec,
       
       Precision
)
where

import Data.Number1.MPFR.FFIhelper as FFIhelper

import Foreign.C(CInt, CLong, CULong, withCString, peekCString)
import Foreign.Marshal(alloca, peekArray)
import Foreign(unsafePerformIO, peek, Ptr, nullPtr, mallocForeignPtrBytes, with, withForeignPtr)

import Data.Bits(shiftL)

import Data.Word(Word)

type Precision = Word


-- these are helper functions, only for internal use
{-# INLINE withMPFRsBA #-}
withMPFRsBA               :: RoundMode -> Precision -> MPFR -> MPFR
                               -> (Ptr MPFR -> Ptr MPFR -> Ptr MPFR -> CRoundMode -> IO CInt)
                               -> (MPFR, Int)
withMPFRsBA r p mp1 mp2 f = unsafePerformIO go
    where go = do ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  let dummy = MP (fromIntegral p) 0 0 fp
                  with dummy $ \p1 -> do
                    with mp1 $ \p2 -> do
                      with mp2 $ \p3 -> do
                          r2 <- f p1 p2 p3 ((fromIntegral . fromEnum) r)
                          r1 <- peekP p1 fp
                          return (r1, fromIntegral r2)

{-# INLINE withMPFRBAui #-}
withMPFRBAui :: RoundMode -> Precision -> MPFR -> CULong
                  ->  (Ptr MPFR -> Ptr MPFR -> CULong -> CRoundMode -> IO CInt)
                  -> (MPFR, Int) 
withMPFRBAui r p mp1 d f = unsafePerformIO go
    where go = do ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  let dummy = MP (fromIntegral p) 0 0 fp
                  with dummy $ \p1 -> do
                    with mp1 $ \p2 -> do
                      r2 <- f p1 p2 d ((fromIntegral . fromEnum) r)
                      r1 <- peekP p1 fp
                      return (r1, fromIntegral r2)
                                
{-# INLINE withMPFRBAsi #-}
withMPFRBAsi             :: RoundMode -> Precision -> MPFR -> CLong
                              -> (Ptr MPFR -> Ptr MPFR -> CLong -> CRoundMode -> IO CInt)
                              -> (MPFR, Int)
withMPFRBAsi r p mp1 d f = unsafePerformIO go 
    where go = do ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  let dummy = MP (fromIntegral p) 0 0 fp
                  with dummy $ \p1 -> do
                    with mp1 $ \p2 -> do
                      r2 <- f p1 p2 d ((fromIntegral . fromEnum) r)
                      r1 <- peekP p1 fp
                      return (r1, fromIntegral r2)
                                  
{-# INLINE withMPFRBAiu #-}
withMPFRBAiu             :: RoundMode -> Precision -> CULong -> MPFR
                              -> (Ptr MPFR -> CULong -> Ptr MPFR -> CRoundMode -> IO CInt)
                              -> (MPFR, Int) 
withMPFRBAiu r p d mp1 f = unsafePerformIO go 
    where go = do ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  let dummy = MP (fromIntegral p) 0 0 fp
                  with dummy $ \p1 -> do
                    with mp1 $ \p2 -> do
                      r2 <- f p1 d p2 ((fromIntegral . fromEnum) r)
                      r1 <- peekP p1 fp
                      return (r1, fromIntegral r2)

{-# INLINE withMPFRBAis #-}
withMPFRBAis             :: RoundMode -> Precision -> CLong -> MPFR
                              -> (Ptr MPFR -> CLong -> Ptr MPFR -> CRoundMode -> IO CInt)
                              -> (MPFR, Int) 
withMPFRBAis r p d mp1 f = unsafePerformIO go
    where go = do ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  let dummy = MP (fromIntegral p) 0 0 fp
                  with dummy $ \p1 -> do
                    with mp1 $ \p2 -> do
                      r2 <- f p1 d p2 ((fromIntegral . fromEnum) r)
                      r1 <- peekP p1 fp
                      return (r1, fromIntegral r2)

{-# INLINE withMPFRB #-}
withMPFRB       :: MPFR -> (Ptr MPFR -> IO CInt) -> CInt 
withMPFRB mp1 f = unsafePerformIO go
    where go = with mp1 $ \p1 -> f p1

withMPFRP       :: MPFR -> (Ptr MPFR -> IO CPrecision) -> CPrecision 
withMPFRP mp1 f = unsafePerformIO go
    where go = with mp1 $ \p1 -> f p1

{-# INLINE withMPFR #-}
withMPFR           :: RoundMode -> Precision -> MPFR 
                        -> (Ptr MPFR -> Ptr MPFR -> CRoundMode -> IO CInt) 
                        -> (MPFR, Int)
withMPFR r p mp1 f = unsafePerformIO go 
    where go = do ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  let dummy = MP (fromIntegral p) 0 0 fp
                  with dummy $ \p1 -> do
                    with mp1 $ \p2 -> do
                      r2 <- f p1 p2 ((fromIntegral . fromEnum) r)
                      r1 <- peekP p1 fp
                      return (r1, fromIntegral r2)
                  
{-# INLINE withMPFRBB #-}
withMPFRBB           :: MPFR -> MPFR 
                          -> (Ptr MPFR -> Ptr MPFR -> IO CInt) 
                          -> CInt  
withMPFRBB mp1 mp2 f = unsafePerformIO go
    where go = do with mp1 $ \p1 -> do 
                    with mp2 $ \p2 -> do 
                                      f p1 p2
                              
{-# INLINE withMPFRC #-}
withMPFRC       :: RoundMode -> Precision ->
                     (Ptr MPFR -> CRoundMode -> IO CInt) -> (MPFR, Int)
withMPFRC r p f = unsafePerformIO go
    where go = do ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  let dummy = MP (fromIntegral p) 0 0 fp
                  with dummy $ \p1 -> do
                    r2 <- f p1 ((fromIntegral . fromEnum) r)
                    r1 <- peekP p1 fp
                    return (r1, fromIntegral r2)
   
withMPFRF         :: MPFR -> RoundMode
                       -> (Ptr MPFR -> CRoundMode -> IO CInt)
                       -> Int
withMPFRF mp1 r f = (fromIntegral . unsafePerformIO) go
    where go = do with mp1 $ \p1 -> f p1 ((fromIntegral . fromEnum) r)

withMPFRUI         :: RoundMode -> Precision -> Word
                        -> (Ptr MPFR -> CULong -> CRoundMode -> IO CInt)
                        -> (MPFR, Int)
withMPFRUI r p d f = unsafePerformIO go 
    where go = do ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  let dummy = MP (fromIntegral p) 0 0 fp
                  with dummy $ \p1 -> do
                    r2 <- f p1 (fromIntegral d) ((fromIntegral . fromEnum) r)
                    r1 <- peekP p1 fp
                    return (r1, fromIntegral r2)

withMPFRR       :: Precision -> MPFR
                     -> (Ptr MPFR -> Ptr MPFR -> IO CInt)
                     -> (MPFR, Int)
withMPFRR p d f = unsafePerformIO go
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

getMantissa'     :: MPFR -> [Limb]
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

--one ::  MPFR              
--one = fromWord Near minPrec 1

--zero :: MPFR              
--ggzero = fromWord Near minPrec 0

minPrec :: Precision
minPrec = 32