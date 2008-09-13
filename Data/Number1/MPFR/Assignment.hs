{-# INCLUDE <mpfr.h> #-}
{-# INCLUDE <chsmpfr.h #-}

module Data.Number1.MPFR.Assignment where

import Data.Number1.MPFR.Internal

set           :: RoundMode -> Precision -> Dyadic -> Dyadic
set r p d = fst $ set_ r p d

set_         :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
set_ r p mp1 = unsafePerformIO go
    where go = do ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  let dummy = MP (fromIntegral p) 0 0 fp
                  with dummy $ \p1 -> do
                    with mp1 $ \p2 -> do 
                      r2 <- mpfr_set p1 p2 ((fromIntegral . fromEnum) r) 
                      r1 <- peekP p1 fp
                      return (r1, fromIntegral r2)

fromWord       :: RoundMode -> Precision -> Word -> Dyadic
fromWord r p d = fst $ fromWord_ r p d

fromInt       :: RoundMode -> Precision -> Int -> Dyadic
fromInt r p d = fst $ fromInt_ r p d

fromDouble       :: RoundMode -> Precision -> Double -> Dyadic
fromDouble r p d = fst $ fromDouble_ r p d

fromWord_       :: RoundMode -> Precision -> Word -> (Dyadic, Int)
fromWord_ r p d = unsafePerformIO go
    where go = do ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  let dummy = MP (fromIntegral p) 0 0 fp
                  with dummy $ \p1 -> do 
                    r2 <- mpfr_set_ui p1 (fromIntegral d) ((fromIntegral . fromEnum) r)
                    r1 <- peekP p1 fp
                    return (r1, fromIntegral r2)                

fromInt_       :: RoundMode -> Precision -> Int -> (Dyadic, Int)
fromInt_ r p d = unsafePerformIO go
    where go = do ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  let dummy = MP (fromIntegral p) 0 0 fp
                  with dummy $ \p1 -> do 
                    r2 <- mpfr_set_si p1 (fromIntegral d) ((fromIntegral . fromEnum) r)
                    r1 <- peekP p1 fp
                    return (r1, fromIntegral r2)

fromDouble_       :: RoundMode -> Precision -> Double -> (Dyadic, Int)
fromDouble_ r p d = unsafePerformIO go
    where go = do ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  let dummy = MP (fromIntegral p) 0 0 fp
                  with dummy $ \p1 -> do 
                    r2 <- mpfr_set_d p1 (realToFrac d) ((fromIntegral . fromEnum) r)
                    r1 <- peekP p1 fp
                    return (r1, fromIntegral r2)
  
-- x * 2 ^ y
int2w         :: RoundMode -> Precision -> Word -> Int -> Dyadic
int2w r p i e = fst $ int2w_ r p i e

int2i         :: RoundMode -> Precision -> Int -> Int -> Dyadic
int2i r p i e = fst $ int2i_ r p i e

int2w_         :: RoundMode -> Precision -> Word -> Int -> (Dyadic, Int)
int2w_ r p i e = unsafePerformIO go
    where go = do ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  let dummy = MP (fromIntegral p) 0 0 fp
                  with dummy $ \p1 -> do
                    r2 <- mpfr_set_ui_2exp p1 (fromIntegral i) (fromIntegral e) ((fromIntegral . fromEnum) r)
                    r1 <- peekP p1 fp
                    return (r1, fromIntegral r2)

int2i_         :: RoundMode -> Precision -> Int -> Int -> (Dyadic, Int)
int2i_ r p i e = unsafePerformIO go
    where go = do ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  let dummy = MP (fromIntegral p) 0 0 fp
                  with dummy $ \p1 -> do
                    r2 <- mpfr_set_si_2exp p1 (fromIntegral i) (fromIntegral e) ((fromIntegral . fromEnum) r)
                    r1 <- peekP p1 fp
                    return (r1, fromIntegral r2)


stringToDyadic         :: RoundMode -> Precision 
                       -> Word -- ^ Base 
                       -> String -> Dyadic
stringToDyadic r p b d = fst $ stringToDyadic_ r p b d

stringToDyadic_         :: RoundMode -> Precision 
                       -> Word -- ^ Base 
                       -> String -> (Dyadic, Int)
stringToDyadic_ r p b d = unsafePerformIO go
    where go = do ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  let dummy = MP (fromIntegral p) 0 0 fp
                  with dummy $ \p1 -> do 
                    withCString d $ \p2 -> do 
                      r2 <- mpfr_set_str p1 p2 (fromIntegral b) ((fromIntegral . fromEnum) r) 
                      r1 <- peekP p1 fp
                      return (r1, fromIntegral r2)

strtofr         :: RoundMode -> Precision
                -> Word -- ^ base
                -> String -> (Dyadic, String)
strtofr r p b d = case strtofr_ r p b d of
                    (a, b, _) -> (a,b)

strtofr_         :: RoundMode -> Precision
                   -> Word -- ^ base
                   -> String -> (Dyadic, String, Int)
strtofr_ r p b d = unsafePerformIO go
    where go = do ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  let dummy = MP (fromIntegral p) 0 0 fp
                  with dummy $ \p1 -> do 
                    withCString d $ \p2 -> do
                      alloca $ \p3 -> do
                        r3 <- mpfr_strtofr p1 p2 p3 (fromIntegral b) ((fromIntegral . fromEnum) p)
                        p3' <- peek p3
                        r2 <- peekCString p3'
                        r1 <- peekP p1 fp
                        return (r1, r2, fromIntegral r3)
                        
                                                                
setInf     :: Precision -> Int -> Dyadic
setInf p i = unsafePerformIO go
    where go = do ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  let dummy = MP (fromIntegral p) 0 0 fp
                  with dummy $ \p1 -> do 
                    mpfr_set_inf p1 (fromIntegral  i)
                    peekP p1 fp

setNaN   :: Precision -> Dyadic
setNaN p = unsafePerformIO go
    where go = do ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  let dummy = MP (fromIntegral p) 0 0 fp
                  with dummy $ \p1 -> do 
                    mpfr_set_nan p1
                    peekP p1 fp

fromIntegerA       :: RoundMode -> Precision -> Integer -> Dyadic
fromIntegerA r p d = stringToDyadic r p 10 (show d)

compose             :: RoundMode -> Precision -> (Integer, Int) -> Dyadic 
compose r p (i, ii) = div2i r p (fromIntegerA r p i) ii

fromString       :: String -> Precision -> Word -> Dyadic
fromString s p b = stringToDyadic Near p b s
