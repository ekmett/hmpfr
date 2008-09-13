{-# LANGUAGE TypeSynonymInstances #-}
{-# INCLUDE <mpfr.h> #-}
{-# INCLUDE <chsmpfr.h> #-}
module Data.Number.MPFR (
-- | This module should always be imported qualified.

-- *** Naming 
-- | - functions ending with _ return a pair (value, rounding indicator). 
--     Rounding indicator indicates whether the result is rounded and in which
--     directon as described in the MPFR manual.
--
-- - the same functions without the _ return just the value. 
--
-- - functions with added \"w\" correspond to MPFR _ui functions
--
-- - functions with added \"i\" correspond to MPFR _si functions


-- *** Equality testing
-- | Equality works as follows: 
-- 
--   - NaN \/= Nan, 
--
--   - Infinity = Infinity, 
--
--   - \-Infinity = -Infinity
--
--   - otherwise normal comparison 

-- *** Ordering      
-- | Ordering works as follows:
-- 
--   - compare NaN _ = GT
--
--   - compare _ NaN = GT
--
--   - infinity < _ = False
--
--   - \-infinity > _ = False
--
--   - NaN [\<,\>,\>=,<=] _ = False
--
--   This mimics the behaviour of built in haskell Float and Double.

-- *** Num instance
-- | Operations defined in Num will be computed so that no precision is lost.

  Dyadic,
  Precision,
  RoundMode(Near, Zero, Up, Down),
  add, sub, mul, div, inverse,
  add_, sub_, mul_, div_,
  addw, addi, mulw, muli, divw, divi, wdiv, idiv, subw, subi, wsub, isub,
  addw_, addi_, mulw_, muli_, divw_, divi_, wdiv_, idiv_, subw_, subi_, wsub_, isub_,
  mul2w, mul2i, div2w, div2i, mul2w_, mul2i_, div2w_, div2i_,
  int2i, int2w, int2i_, int2w_,
  fma, fms, fma_, fms_, nextBelow,
  sqr, sqrt, root, pow, poww, powi, wpoww, wpow, 
  sqr_, sqrt_, root_, pow_, poww_, powi_, wpoww_, wpow_,
  exp, exp2, exp10, log, log2, log10, sinh, cosh, tanh,
  exp_, exp2_, exp10_, log_, log2_, log10_, sinh_, cosh_, tanh_,
  neg, absD, dim, neg_, absD_, dim_, 
  isNaN, isInfinite, isNumber, isZero, greater, greatereq, less, lesseq,
  equal, maxD, minD, maxD_, minD_, sgn, 
  dyadicToDouble, dyadicToWord, dyadicToInt, dyadicToString, decompose, toStringExp, toString,
  pi, log2c, euler, catalan, pi_, log2c_, euler_, catalan_,
  set, set_,
  fromDouble, fromInt, fromWord, fromDouble_, fromInt_, fromWord_, fromIntegerA, compose, fromString,
  getPrec, getMantissa, getExp, 
  minPrec, one, zero, addPrec
) where



import Data.Number.FFIhelper

import Foreign.C(CInt, CLong, CULong, withCString, peekCString)
import Foreign.Marshal(alloca, peekArray)
import Foreign(unsafePerformIO, peek, Ptr, mallocForeignPtrBytes, with)

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
   
withDyadicF       :: Dyadic -> RoundMode
                     -> (Ptr MPFR_T -> CRoundMode -> IO CInt)
                     -> Int
withDyadicF mp1 r = (fromIntegral . unsafePerformIO) go
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
withDyadicR r d f = unsafePerformIO go
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


--------------------------------------------------------------------
-- assignment functions

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

strtofr         :: RoundMode -> Precision -> 
                -> Word -- ^ base
                -> String -> (Dyadic, String)
strtofr r p b d = case strtofr_ r p b d of
                    (a, b, _) -> (a,b)

strtofr_         :: RoundMode -> Precision -> 
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
                    peekP p1

setNaN   :: Precision -> Dyadic
setNaN p = unsafePerformIO go
    where go = do ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  let dummy = MP (fromIntegral p) 0 0 fp
                  with dummy $ \p1 -> do 
                    mpfr_set_nan p1
                    peekP p1

-------------------------------------------------------------------
-- conversion functions

toDouble       :: RoundMode -> Dyadic -> Double
toDouble r mp1 = (realToFrac . unsafePerformIO) go
    where go = with mp1 $ \p -> mpfr_get_d p ((fromIntegral . fromEnum) r)

toDouble2exp     :: RoundMode -> Dyadic -> (Double, Int)
toDouble2exp r mp1 = unsafePerfromIO go 
    where go = do with mp1 $ \p1 -> do
                    alloca $ \p2 -> do
                      r1 <- mpfr_get_d_2exp p2 p1 ((fromIntegral . fromEnum) r)
                      r2 <- peek p2
                      return (r1, fromIntegral r2)
                      
toInt     :: RoundMode -> Dyadic -> Int
toInt r mp1 = (fromIntegral . unsafePerformIO) go
    where go = with mp1 $ \p -> mpfr_get_si p ((fromIntegral . fromEnum) r)

toWord       :: RoundMode -> Dyadic -> Word
toWord r mp1 = (fromIntegral . unsafePerformIO) go
    where go = with mp1 $ \p -> mpfr_get_ui p ((fromIntegral . fromEnum) r)


toString           :: RoundMode 
                   -> Word -- ^ number of decimals
                   -> Word -- ^ base
                   -> Dyadic -> (String, Exp)
toString r n b mp1 = unsafePerformIO go 
    where go = with mp1 $ \p1 -> do
                 alloca $ \p2 -> do
                     p3 <- mpfr_get_str nullPtr p2 (fromIntegral b) (fromIntegral n) p1 ((fromIntegral . fromEnum) r)
                     r1 <- peekCString p3 
                     r2 <- peek p2
                     mpfr_free_str p3
                     return (r1, r2)

fitsULong     :: RoundMode -> Dyadic -> Bool
fitsULong r d = withDyadicF d r mpfr_fits_ulong_p /= 0 

fitsSLong     :: RoundMode -> Dyadic -> Bool
fitsSLong r d = withDyadicF d r mpfr_fits_slong_p /= 0 

fitsUInt     :: RoundMode -> Dyadic -> Bool
fitsUInt r d = withDyadicF d r mpfr_fits_uint_p /= 0 

fitsSInt     :: RoundMode -> Dyadic -> Bool
fitsSInt r d = withDyadicF d r mpfr_fits_sint_p /= 0 

fitsULong     :: RoundMode -> Dyadic -> Bool
fitsULong r d = withDyadicF d r mpfr_fits_ushort_p /= 0 

fitsSShort     :: RoundMode -> Dyadic -> Bool
fitsSShort r d = withDyadicF d r mpfr_fits_sshort_p /= 0 

-- TODO
decompose   :: Dyadic -> (Integer, Int)
decompose d = (getMantissa d, getExp d - Prelude.ceiling (fromIntegral (getPrec d) / fromIntegral bitsPerMPLimb :: Double) * bitsPerMPLimb)

-- basic arithmetic operations

add           :: RoundMode -> Precision -> Dyadic -> Dyadic -> Dyadic
add r p d1 d2 = fst $ add_ r p d1 d2 

addw          :: RoundMode -> Precision -> Dyadic -> Word -> Dyadic
addw r p d1 d = fst $ addw_ r p d1 d 

addi          :: RoundMode -> Precision -> Dyadic -> Int -> Dyadic
addi r p d1 d = fst $ addi_ r p d1 d 

sub           :: RoundMode -> Precision -> Dyadic -> Dyadic -> Dyadic
sub r p d1 d2 = fst $ sub_ r p d1 d2

subw          :: RoundMode -> Precision -> Dyadic -> Word -> Dyadic
subw r p d1 d = fst $ subw_ r p d1 d 

subi          :: RoundMode -> Precision -> Dyadic -> Int -> Dyadic
subi r p d1 d = fst $ subi_ r p d1 d 

wsub          :: RoundMode -> Precision -> Word -> Dyadic -> Dyadic
wsub r p d d1 = fst $ wsub_ r p d d1 

isub          :: RoundMode -> Precision -> Int -> Dyadic -> Dyadic
isub r p d d1 = fst $ isub_ r p d d1 

mul           :: RoundMode -> Precision -> Dyadic -> Dyadic -> Dyadic
mul r p d1 d2 = fst $ mul_ r p d1 d2

mulw          :: RoundMode -> Precision -> Dyadic -> Word -> Dyadic
mulw r p d1 d = fst $ mulw_ r p d1 d 

muli          :: RoundMode -> Precision -> Dyadic -> Int -> Dyadic
muli r p d1 d = fst $ muli_ r p d1 d 

sqr       :: RoundMode -> Precision -> Dyadic -> Dyadic 
sqr r p d = fst $ sqr_ r p d

div           :: RoundMode -> Precision -> Dyadic -> Dyadic -> Dyadic
div r p d1 d2 = fst $ div_ r p d1 d2

divw          :: RoundMode -> Precision -> Dyadic -> Word -> Dyadic
divw r p d1 d = fst $ divw_ r p d1 d 

divi          :: RoundMode -> Precision -> Dyadic -> Int -> Dyadic
divi r p d1 d = fst $ divi_ r p d1 d 

wdiv          :: RoundMode -> Precision -> Word -> Dyadic -> Dyadic
wdiv r p d d1 = fst $ wdiv_ r p d d1 

idiv          :: RoundMode -> Precision -> Int -> Dyadic -> Dyadic
idiv r p d d1 = fst $ idiv_ r p d d1 

sqrt       :: RoundMode -> Precision -> Dyadic -> Dyadic
sqrt r p d = fst $ sqrt_ r p d

sqrtw       :: RoundMode -> Precision -> Word -> Dyadic
sqrtw r p d = fst $ sqrtw_ r p d

cbrt       :: RoundMode -> Precision -> Dyadic -> Dyadic
cbrt r p d = fst $ cbrt_ r p d

root         :: RoundMode -> Precision -> Dyadic -> Word -> Dyadic
root r p d n = fst $ root_ r p d n

pow           :: RoundMode -> Precision -> Dyadic -> Dyadic -> Dyadic
pow r p d1 d2 = fst $ pow_ r p d1 d2 

poww           :: RoundMode -> Precision -> Dyadic -> Word -> Dyadic 
poww r p d1 d2 = fst $ poww_ r p d1 d2

powi           :: RoundMode -> Precision -> Dyadic -> Int -> Dyadic 
powi r p d1 d2 = fst $ powi_ r p d1 d2

wpoww           :: RoundMode -> Precision -> Word -> Word -> Dyadic 
wpoww r p d1 d2 = fst $ wpoww_ r p d1 d2

wpow           :: RoundMode -> Precision -> Word -> Dyadic -> Dyadic 
wpow r p d1 d2 = fst $ wpow_ r p d1 d2

neg       :: RoundMode -> Precision -> Dyadic -> Dyadic
neg r p d = fst $ neg_ r p d

absD      :: RoundMode -> Precision -> Dyadic -> Dyadic 
absD r p d = fst $ absD_ r p d

dim           :: RoundMode -> Precision -> Dyadic -> Dyadic -> Dyadic
dim r p d1 d2 = fst $ dim_ r p d1 d2 

mul2w           :: RoundMode -> Precision -> Dyadic -> Word -> Dyadic
mul2w r p d1 d2 = fst $ mul2w_ r p d1 d2

mul2i          :: RoundMode -> Precision -> Dyadic -> Int -> Dyadic
mul2i r p d1 d2 = fst $ mul2i_ r p d1 d2

div2w          :: RoundMode -> Precision -> Dyadic -> Word -> Dyadic
div2w r p d1 d2 = fst $ div2w_ r p d1 d2

div2i          :: RoundMode -> Precision -> Dyadic -> Int -> Dyadic
div2i r p d1 d2 = fst $ div2i_ r p d1 d2

add_           :: RoundMode -> Precision -> Dyadic -> Dyadic -> (Dyadic,Int)
add_ r p d1 d2 =  withDyadicsBA r p d1 d2 mpfr_add

addw_          :: RoundMode -> Precision -> Dyadic -> Word -> (Dyadic, Int)
addw_ r p d1 d = withDyadicBAui r p d1 (fromIntegral d) mpfr_add_ui

addi_          :: RoundMode -> Precision -> Dyadic -> Int -> (Dyadic, Int)
addi_ r p d1 d = withDyadicBAsi r p d1 (fromIntegral d) mpfr_add_si

sub_           :: RoundMode -> Precision -> Dyadic -> Dyadic -> (Dyadic,Int)
sub_ r p d1 d2 =  withDyadicsBA r p d1 d2 mpfr_sub

subw_          :: RoundMode -> Precision -> Dyadic -> Word -> (Dyadic, Int)
subw_ r p d1 d = withDyadicBAui r p d1 (fromIntegral d) mpfr_sub_ui

subi_          :: RoundMode -> Precision -> Dyadic -> Int -> (Dyadic, Int)
subi_ r p d1 d = withDyadicBAsi r p d1 (fromIntegral d) mpfr_sub_si

wsub_          :: RoundMode -> Precision -> Word -> Dyadic -> (Dyadic, Int)
wsub_ r p d d1 = withDyadicBAiu r p (fromIntegral d) d1 mpfr_ui_sub

isub_          :: RoundMode -> Precision -> Int -> Dyadic -> (Dyadic, Int)
isub_ r p d d1 = withDyadicBAis r p (fromIntegral d) d1 mpfr_si_sub

mul_           :: RoundMode -> Precision -> Dyadic -> Dyadic -> (Dyadic,Int)
mul_ r p d1 d2 =  withDyadicsBA r p d1 d2 mpfr_mul

mulw_          :: RoundMode -> Precision -> Dyadic -> Word -> (Dyadic, Int)
mulw_ r p d1 d = withDyadicBAui r p d1 (fromIntegral d) mpfr_mul_ui

muli_          :: RoundMode -> Precision -> Dyadic -> Int -> (Dyadic, Int)
muli_ r p d1 d = withDyadicBAsi r p d1 (fromIntegral d) mpfr_mul_si

sqr_       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
sqr_ r p d = withDyadic r p d mpfr_sqr

div_           :: RoundMode -> Precision -> Dyadic -> Dyadic -> (Dyadic, Int)
div_ r p d1 d2 =  withDyadicsBA r p d1 d2 mpfr_div

divw_          :: RoundMode -> Precision -> Dyadic -> Word -> (Dyadic, Int)
divw_ r p d1 d = withDyadicBAui r p d1 (fromIntegral d) mpfr_div_ui

divi_          :: RoundMode -> Precision -> Dyadic -> Int -> (Dyadic, Int)
divi_ r p d1 d = withDyadicBAsi r p d1 (fromIntegral d) mpfr_div_si

wdiv_          :: RoundMode -> Precision -> Word -> Dyadic -> (Dyadic, Int)
wdiv_ r p d d1 = withDyadicBAiu r p (fromIntegral d) d1 mpfr_ui_div

idiv_          :: RoundMode -> Precision -> Int -> Dyadic -> (Dyadic, Int)
idiv_ r p d d1 = withDyadicBAis r p (fromIntegral d) d1 mpfr_si_div

sqrt_       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
sqrt_ r p d = withDyadic r p d mpfr_sqrt
 
sqrtw_       :: RoundMode -> Precision -> Word -> (Dyadic, Int)
sqrtw_ r p d = withDyadicUI r p d mpfr_sqrt_ui

cbrt_       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
cbrt_ r p d = withDyadic r p d mpfr_cbrt

root_        :: RoundMode -> Precision -> Dyadic -> Word -> (Dyadic, Int)
root_ r p d n = withDyadicBAui r p d (fromIntegral n) mpfr_root

pow_          :: RoundMode -> Precision -> Dyadic -> Dyadic -> (Dyadic, Int)
pow_ r p d1 d2 = withDyadicsBA r p d1 d2 mpfr_pow 

poww_          :: RoundMode -> Precision -> Dyadic -> Word -> (Dyadic , Int)
poww_ r p d1 d2 = withDyadicBAui r p d1 (fromIntegral d2) mpfr_pow_ui

powi_           :: RoundMode -> Precision -> Dyadic -> Int -> (Dyadic , Int)
powi_ r p d1 d2 = withDyadicBAsi r p d1 (fromIntegral d2) mpfr_pow_si

wpoww_          :: RoundMode -> Precision -> Word -> Word -> (Dyadic , Int)
wpoww_ r p d1 d2 = unsafePerformIO go
    where go = do ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  let dummy = MP (fromIntegral p) 0 0 fp
                  with dummy $ \p1 -> do 
                    r2 <- mpfr_ui_pow_ui p1 (fromIntegral d1) (fromIntegral d2) ((fromIntegral . fromEnum) r)
                    r1 <- peekP p1 fp
                    return (r1, fromIntegral r2)
        
wpow_           :: RoundMode -> Precision -> Word -> Dyadic -> (Dyadic , Int)
wpow_ r p d1 d2 = withDyadicBAiu r p (fromIntegral d1) d2 mpfr_ui_pow

neg_       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
neg_ r p d = withDyadic r p d mpfr_neg

absD_      :: RoundMode -> Precision -> Dyadic -> (Dyadic , Int)
absD_ r p d = withDyadic r p d mpfr_abs

dim_           :: RoundMode -> Precision -> Dyadic -> Dyadic -> (Dyadic, Int)
dim_ r p d1 d2 = withDyadicsBA r p d1 d2 mpfr_dim

mul2w_           :: RoundMode -> Precision -> Dyadic -> Word -> (Dyadic, Int)
mul2w_ r p d1 d2 = withDyadicBAui r p d1 (fromIntegral d2) mpfr_mul_2ui

mul2i_          :: RoundMode -> Precision -> Dyadic -> Int -> (Dyadic, Int)
mul2i_ r p d1 d2 = withDyadicBAsi r p d1 (fromIntegral d2) mpfr_mul_2si

div2w_          :: RoundMode -> Precision -> Dyadic -> Word -> (Dyadic, Int)
div2w_ r p d1 d2 = withDyadicBAui r p d1 (fromIntegral d2) mpfr_div_2ui

div2i_          :: RoundMode -> Precision -> Dyadic -> Int -> (Dyadic, Int)
div2i_ r p d1 d2 = withDyadicBAsi r p d1 (fromIntegral d2) mpfr_div_2si

-------------------------------------------------------------------------
-- comparison functions
cmp         :: Dyadic -> Dyadic -> Maybe Ordering
cmp mp1 mp2 = if MPFR.isNaN mp1 || MPFR.isNaN mp2 then Nothing 
                else compare (withDyadicBB mp1 mp2 mpfr_cmp) 0

cmpw       :: Dyadic -> Word -> Maybe Ordering
cmpw mp1 w = if MPFR.isNaN mp1 then Nothing else Just (compare (unsafePerformIO go) 0)
    where go = with mp1 $ \p -> mpfr_cmp_ui p (fromIntegral w) 

cmpw       :: Dyadic -> Int -> Maybe Ordering
cmpw mp1 i = if MPFR.isNaN mp1 then Nothing else Just (compare (unsafePerformIO go) 0)
    where go = with mp1 $ \p -> mpfr_cmp_si p (fromIntegral i)

cmpd       :: Dyadic -> Double -> Maybe Ordering
cmpd mp1 d = if MPFR.isNaN mp1 || Prelude.isNaN d then Nothing 
               else Just (compare (unsafePerformIO go) 0)
    where go = with mp1 $ \p -> mpfr_cmp_d p (realToFrac d)
                    
cmp2w       :: Dyadic -> Word -> Exp -> Maybe Ordering
cmp2w d w e = unsafePerformIO go
    where go = do mpfr_clear_erangeflag
                  with d $ \p -> do
                    r1 <- mpfr_cmp_ui_2exp p (fromIntegral w) e
                    r2 <- mpfr_erangeflag_p
                    if r2 == 0 then return (Just (compare r1 0))
                      else do mpfr_clear_erangeflag
                              return Nothing

cmp2i       :: Dyadic -> Int -> Exp -> Maybe Ordering
cmp2i d w e = unsafePerformIO go
    where go = do mpfr_clear_erangeflag
                  with d $ \p -> do
                    r1 <- mpfr_cmp_si_2exp p (fromIntegral w) e
                    r2 <- mpfr_erangeflag_p
                    if r2 == 0 then return (Just (compare r1 0))
                      else do mpfr_clear_erangeflag
                              return Nothing
cmpabs         :: Dyadic -> Dyadic -> Maybe Ordering
cmpabs mp1 mp2 = if MPFR.isNaN mp1 || MPFR.isNaN mp2 then Nothing 
                   else compare (withDyadicBB mp1 mp2 mpfr_cmpabs) 0

isNaN   :: Dyadic -> Bool
isNaN d = withDyadicB d mpfr_nan_p /= 0

isInfinite   :: Dyadic -> Bool
isInfinite d = withDyadicB d mpfr_inf_p /= 0 

isNumber   :: Dyadic -> Bool
isNumber d = withDyadicB d mpfr_number_p /= 0 

isZero   :: Dyadic -> Bool
isZero d = withDyadicB d mpfr_zero_p /= 0

sgn     :: Dyadic -> Maybe Int 
sgn mp1 = case (cmpw mp1 0) of
            Nothing -> Nothing
            Just x -> Just (pred . fromEnum $ x)

-- TODO Maybe Bool????
greater       :: Dyadic -> Dyadic -> Bool
greater d1 d2 = withDyadicBB d1 d2 mpfr_greater_p /= 0

greatereq       :: Dyadic -> Dyadic -> Bool
greatereq d1 d2 = withDyadicBB d1 d2 mpfr_greaterequal_p /= 0

less       :: Dyadic -> Dyadic -> Bool
less d1 d2 = withDyadicBB d1 d2 mpfr_less_p /= 0

lesseq       :: Dyadic -> Dyadic -> Bool
lesseq d1 d2 = withDyadicBB d1 d2 mpfr_lessequal_p /= 0

lessgreater       :: Dyadic -> Dyadic -> Maybe Bool
lessgreater d1 d2 = if MPFR.isNaN d1 || MPFR.isNaN d2 then Nothing 
                      else Just (withDyadicBB d1 d2 mpfr_lessgreater_p /= 0)

equal       :: Dyadic -> Dyadic -> Bool
equal d1 d2 = withDyadicBB d1 d2 mpfr_equal_p /= 0

unordered       :: Dyadic -> Dyadic -> Maybe Bool
unordered d1 d2 = if MPFR.isNaN d1 || MPFR.isNaN d2 then Nothing 
                    else Just (withDyadicBB d1 d2 mpfr_unordered_p /= 0)

--------------------------------------------------------------------------------
-- special functions

log       :: RoundMode -> Precision -> Dyadic -> Dyadic
log r p d = fst $ log_ r p d

log2       :: RoundMode -> Precision -> Dyadic -> Dyadic
log2 r p d = fst $ log2_ r p d

log10       :: RoundMode -> Precision -> Dyadic -> Dyadic
log10 r p d = fst $ log10_ r p d

exp       :: RoundMode -> Precision -> Dyadic -> Dyadic
exp r p d = fst $ exp_ r p d

exp2       :: RoundMode -> Precision -> Dyadic -> Dyadic
exp2 r p d = fst $ exp2_ r p d

exp10       :: RoundMode -> Precision -> Dyadic -> Dyadic
exp10 r p d = fst $ exp10_ r p d

sin       :: RoundMode -> Precision -> Dyadic -> Dyadic
sin r p d = fst $ sin_ r p d

cos       :: RoundMode -> Precision -> Dyadic -> Dyadic
cos r p d = fst $ cos_ r p d

tan       :: RoundMode -> Precision -> Dyadic -> Dyadic
tan r p d = fst $ tan_ r p d 

sec       :: RoundMode -> Precision -> Dyadic -> Dyadic
sec r p d = fst $ sec_ r p d

csc       :: RoundMode -> Precision -> Dyadic -> Dyadic
csc r p d = fst $ csc_ r p d

cot       :: RoundMode -> Precision -> Dyadic -> Dyadic
cot r p d = fst $ cot_ r p d 

sincos :: RoundMode
       -> Precision -- ^ precision to compute sin
       -> Precision -- ^ precision to compute cos 
       -> Dyadic
       -> (Dyadic, Dyadic) -- ^ return (sin x, cos x)
sincos r p p' d = case sincos_ r p d' d of
                    (a, b, _) -> (a, b)

asin       :: RoundMode -> Precision -> Dyadic -> Dyadic
asin r p d = fst $ asin_ r p d

acos       :: RoundMode -> Precision -> Dyadic -> Dyadic
acos r p d = fst $ acos_ r p d

atan       :: RoundMode -> Precision -> Dyadic -> Dyadic
atan r p d = fst $ atan_ r p d 

atan2          :: RoundMode -> Precision -> Dyadic -> Dyadic -> Dyadic
atan2 r p d d' = fst $ atan2_ r p d d'

sinh       :: RoundMode -> Precision -> Dyadic -> Dyadic
sinh r p d = fst $ sinh_ r p d

cosh       :: RoundMode -> Precision -> Dyadic -> Dyadic
cosh r p d = fst $ cosh_ r p d

tanh       :: RoundMode -> Precision -> Dyadic -> Dyadic
tanh r p d = fst $ tanh_ r p d 

sech       :: RoundMode -> Precision -> Dyadic -> Dyadic
sech r p d = fst $ sech_ r p d

csch       :: RoundMode -> Precision -> Dyadic -> Dyadic
csch r p d = fst $ csch_ r p d

coth       :: RoundMode -> Precision -> Dyadic -> Dyadic
coth r p d = fst $ coth_ r p d 

acosh       :: RoundMode -> Precision -> Dyadic -> Dyadic
acosh r p d = fst $ acosh_ r p d

asinh       :: RoundMode -> Precision -> Dyadic -> Dyadic
asinh r p d = fst $ asinh_ r p d

atanh       :: RoundMode -> Precision -> Dyadic -> Dyadic
atanh r p d = fst $ atanh_ r p d 

facw       :: RoundMode -> Precision -> Word -> Dyadic
facw r p d = fst $ facw_ r p d

log1p       :: RoundMode -> Precision -> Dyadic -> Dyadic
log1p r p d = fst $ log1p_ r p d

expm1       :: RoundMode -> Precision -> Dyadic -> Dyadic
expm1 r p d = fst $ expm1_ r p d

eint       :: RoundMode -> Precision -> Dyadic -> Dyadic
eint r p d = fst $ eint_ r p d

gamma       :: RoundMode -> Precision -> Dyadic -> Dyadic
gamma r p d = fst $ gamma_ r p d

lngamma       :: RoundMode -> Precision -> Dyadic -> Dyadic
lngamma r p d = fst $ lngamma_ r p d

lgamma       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
lgamma r p d = case lgamma_ r p d of 
                 (a, b, _) -> (a,b)

zeta       :: RoundMode -> Precision -> Dyadic -> Dyadic
zeta r p d = fst $ zeta_ r p d

zetaw       :: RoundMode -> Precision -> Word -> Dyadic
zetaw r p d = fst $ zetaw_ r p d

erf       :: RoundMode -> Precision -> Dyadic -> Dyadic
erf r p d = fst $ erf_ r p d

erfc       :: RoundMode -> Precision -> Dyadic -> Dyadic
erfc r p d = fst $ erfc_ r p d

j0       :: RoundMode -> Precision -> Dyadic -> Dyadic
j0 r p d = fst $ j0_ r p d

j1       :: RoundMode -> Precision -> Dyadic -> Dyadic
j1 r p d = fst $ j1_ r p d

jn         :: RoundMode -> Precision -> Int -> Dyadic -> Dyadic
jn r p w d = fst $ jn_ r p w d

y0       :: RoundMode -> Precision -> Dyadic -> Dyadic
y0 r p d = fst $ y0_ r p d

y1       :: RoundMode -> Precision -> Dyadic -> Dyadic
y1 r p d = fst $ y1_ r p d

yn         :: RoundMode -> Precision -> Word -> Dyadic -> Dyadic
yn r p w d = fst $ yn_ r p w d

fma              :: RoundMode -> Precision -> Dyadic -> Dyadic -> Dyadic -> Dyadic
fma r p d1 d2 d3 = fst $ fma_ r p d1 d2 d3

fms              :: RoundMode -> Precision -> Dyadic -> Dyadic -> Dyadic -> Dyadic
fms r p d1 d2 d3 = fst $ fms_ r p d1 d2 d3

agm           :: RoundMode -> Precision -> Dyadic -> Dyadic -> Dyadic
agm r p d1 d2 = fst $ agm_ r p d1 d2

hypot           :: RoundMode -> Precision -> Dyadic -> Dyadic -> Dyadic
hypot r p d1 d2 = fst $ hypot_ r p d1 d2

pi :: RoundMode -> Precision -> Dyadic
pi r p = fst $ pi_ r p

log2c     :: RoundMode -> Precision -> Dyadic
log2c r p = fst $ pi_ r p

euler     :: RoundMode -> Precision -> Dyadic
euler r p = fst $ pi_ r p

catalan     :: RoundMode -> Precision -> Dyadic
catalan r p = fst $ pi_ r p


log_       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
log_ r p d = withDyadic r p d mpfr_log

log2_       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
log2_ r p d = withDyadic r p d mpfr_log2

log10_       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
log10_ r p d = withDyadic r p d mpfr_log10

exp_       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
exp_ r p d = withDyadic r p d mpfr_exp

exp2_       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
exp2_ r p d = withDyadic r p d mpfr_exp2

exp10_       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
exp10_ r p d = withDyadic r p d mpfr_exp10

sin_       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
sin_ r p d = withDyadic r p d mpfr_sin

cos_       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
cos_ r p d = withDyadic r p d mpfr_cos

tan_       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
tan_ r p d = withDyadic r p d mpfr_tan

sec_       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
sec_ r p d = withDyadic r p d mpfr_sec

csc_       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
csc_ r p d = withDyadic r p d mpfr_csc

cot_       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
cot_ r p d = withDyadic r p d mpfr_cot


sincos_ :: RoundMode
        -> Precision -- ^ precision to compute sin
        -> Precision -- ^ precision to compute cos 
        -> Dyadic
        -> (Dyadic, Dyadic)
sincos r p p' d = unsafePerformIO go 
    where go = do ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  let dummy = MP (fromIntegral p) 0 0 fp
                  ls' <- mpfr_custom_get_size (fromIntegral p')
                  fp' <- mallocForeignPtrBytes (fromIntegral ls')
                  let dummy' = MP (fromIntegral p') 0 0 fp'
                  with dummy $ \p1 -> do 
                    with dummy' $ \p2 -> do 
                      with d $ \p3 -> do
                        r3 <- mpfr_sin_cos p1 p2 p3 ((fromIntegral . fromEnum) r)
                        r1 <- peekP p1 fp
                        r2 <- peekP p2 fp'
                        return (r1, r2, fromIntegral r3)

asin_       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
asin_ r p d = withDyadic r p d mpfr_asin

acos_       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
acos_ r p d = withDyadic r p d mpfr_acos

atan_       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
atan_ r p d = withDyadic r p d mpfr_atan

atan2_ :: RoundMode -> Precision -> Dyadic -> Dyadic -> (Dyadic, Int)
atan2_ r p d d' = withDyadicsBA r p d d' 

sinh_       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
sinh_ r p d = withDyadic r p d mpfr_sinh

cosh_       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
cosh_ r p d = withDyadic r p d mpfr_cosh

tanh_       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
tanh_ r p d = withDyadic r p d mpfr_tanh

sech_       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
sech_ r p d = withDyadic r p d mpfr_sech

csch_       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
csch_ r p d = withDyadic r p d mpfr_csch

coth_       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
coth_ r p d = withDyadic r p d mpfr_coth

acosh_       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
acosh_ r p d = withDyadic r p d mpfr_acosh

asinh_       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
asinh_ r p d = withDyadic r p d mpfr_asinh

atanh_       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
atanh_ r p d = withDyadic r p d mpfr_atanh

facw_       :: RoundMode -> Precision -> Word -> (Dyadic, Int)
facw_ r p w = withDyadicUI r p w mpfr_fac_ui

log1p_       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
log1p_ r p d = withDyadic r p d mpfr_log1p

expm1_       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
expm1_ r p d = withDyadic r p d mpfr_expm1

eint_       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
eint_ r p d = withDyadic r p d mpfr_eint

gamma_       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
gamma_ r p d = withDyadic r p d mpfr_gamma

lngamma_       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
lngamma_ r p d = withDyadic r p d mpfr_lngamma

lgamma_       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int, Int)
lgamma_ r p d = unsafePerformIO go
    where go = do ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  let dummy = MP (fromIntegral p) 0 0 fp
                  with dummy $ \p1 -> do
                    with d $ \p2 -> do
                      alloca $ \p3 -> do
                        r3 <- mpfr_lgamma p1 p3 p2 ((fromIntegral . fromEnum) r)
                        r2 <- peek p3
                        r1 <- peekP p1 fp
                        return (r1, fromIntegral r2, fromIntegral r3)
                    
zeta_       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
zeta_ r p d = withDyadic r p d mpfr_zeta

zetaw_       :: RoundMode -> Precision -> Word -> (Dyadic, Int)
zetaw_ r p d = withDyadicUI r p d mpfr_zeta_ui

erf_       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
erf_ r p d = withDyadic r p d mpfr_erf

erfc_       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
erfc_ r p d = withDyadic r p d mpfr_erfc

j0_       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
j0_ r p d = withDyadic r p d mpfr_j0

j1_       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
j1_ r p d = withDyadic r p d mpfr_j1

jn_         :: RoundMode -> Precision -> Int -> Dyadic -> (Dyadic, Int)
jn_ r p i d = withDyadicBAis r p (fromIntegral i) d mpfr_jn

y0_       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
y0_ r p d = withDyadic r p d mpfr_y0

y1_       :: RoundMode -> Precision -> Dyadic -> (Dyadic, Int)
y1_ r p d = withDyadic r p d mpfr_y1

yn_         :: RoundMode -> Precision -> Int -> Dyadic -> (Dyadic, Int)
yn_ r p i d = withDyadicBAis r p (fromIntegral i) d mpfr_yn

fma_                 :: RoundMode -> Precision -> Dyadic -> Dyadic -> Dyadic -> (Dyadic, Int)
fma_ r p mp1 mp2 mp3 = unsafePerformIO go
    where go = do ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  let dummy = MP (fromIntegral p) 0 0 fp
                  with dummy $ \p1 -> do
                    with mp1 $ \p2 -> do 
                      with mp2 $ \p3 -> do 
                        with mp3 $ \p4 -> do 
                          r2 <- mpfr_fma p1 p2 p3 p4 ((fromIntegral . fromEnum) r) 
                          r1 <- peekP p1 fp 
                          return (r1, fromIntegral r2)

fms_                 :: RoundMode -> Precision -> Dyadic -> Dyadic -> Dyadic -> (Dyadic, Int)
fms_ r p mp1 mp2 mp3 = unsafePerformIO go
    where go = do ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  let dummy = MP (fromIntegral p) 0 0 fp
                  with dummy $ \p1 -> do
                    with mp1 $ \p2 -> do 
                      with mp2 $ \p3 -> do 
                        with mp3 $ \p4 -> do 
                          r2 <- mpfr_fms p1 p2 p3 p4 ((fromIntegral . fromEnum) r) 
                          r1 <- peekP p1 fp
                          return (r1, fromIntegral r2)

agm_           :: RoundMode -> Precision -> Dyadic -> Dyadic -> (Dyadic,Int)
agm_ r p d1 d2 =  withDyadicsBA r p d1 d2 mpfr_agm

hypot_           :: RoundMode -> Precision -> Dyadic -> Dyadic -> (Dyadic,Int)
hypot_ r p d1 d2 =  withDyadicsBA r p d1 d2 mpfr_hypot

pi_     :: RoundMode -> Precision -> (Dyadic, Int)
pi_ r p = withDyadicC r p mpfr_const_pi

log2c_     :: RoundMode -> Precision -> (Dyadic, Int)
log2c_ r p = withDyadicC r p mpfr_const_log2

euler_     :: RoundMode -> Precision -> (Dyadic, Int)
euler_ r p = withDyadicC r p mpfr_const_euler

catalan_     :: RoundMode -> Precision -> (Dyadic, Int)
catalan_ r p = withDyadicC r p mpfr_const_catalan

freeCache :: IO ()
freeCache = mpfr_free_cache

-- TODO mpfr_sum
-----------------------------------------------------------------------------------------
-- integer related functions
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
remainder_ r p d d' = withDyadicBA r p d d' mpfr_remainder

remquo_          :: RoundMode -> Precision -> Dyadic -> Dyadic -> (Dyadic, Int, Int)
remquo_ r p d d' = unsafePerformIO go
    where go = do ls <- mpfr_custom_get_size p
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  let dummy = MP p 0 0 fp
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

--------------------------------------------------------------------------------
-- mischellaneous functions
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
random2 p m e = do let p = fromIntegral (getPrec mp1)
                   ls <- mpfr_custom_get_size p
                   fp <- mallocForeignPtrBytes (fromIntegral ls)
                   let dummy = MP p 0 0 fp
                   with dummy $ \p1 -> do
                     mpfr_random2 p1 m e
                     peekP p1 fp

getExp   :: Dyadic -> Exp
getExp d = (fromIntegral . unsafePerformIO) go
                 where go = do with d $ \p1 -> mpfr_custom_get_exp p1

setExp     :: Dyadic -> Exp -> Dyadic
setExp d e = unsafePerformIO go
    where go = do let p = fromIntegral (getPrec mp1)
                  ls <- mpfr_custom_get_size p
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  let dummy = MP p 0 0 fp
                  with dummy $ \p1 -> do
                    with d $ \p2 -> do mpfr_set p1 p2
                      mpfr_set_exp p1 e
                      peekP p1 fp

signbit   :: Dyadic -> Bool
signbit d = withDyadicB d mpfr_signbit

maxD_           :: RoundMode -> Precision -> Dyadic -> Dyadic -> (Dyadic, Int)
maxD_ r p d1 d2 = withDyadicsBA r p d1 d2 mpfr_max

minD_           :: RoundMode -> Precision -> Dyadic -> Dyadic -> (Dyadic, Int)
minD_ r p d1 d2 = withDyadicsBA r p d1 d2 mpfr_min

----------------------------------------------------------
-- powers

-----------------------------------------------------------

-- transcendental functions

------------------------------------------------------------

--------------------------------------------------------

--  comparison functions

sgn   :: Dyadic -> Int 
sgn d = case compare zero d of
          LT -> 1
          EQ -> 0
          _  -> -1

--conversion from dyadics to basic haskell types

toStringExp       :: Word -> Dyadic -> String
toStringExp dec d = s ++ case e > 0 of
                           True  -> case Prelude.floor (logBase 10 2 * fromIntegral (getExp d) :: Double) > dec  of
                                      False -> take e ss ++ let bt = backtrim (drop e ss) in if null bt then "" else "." ++ bt
                                      True  -> head ss : "." ++ let bt = (backtrim . tail) ss in if null bt then "0"
                                                                                                   else bt ++ "e" ++ show (pred e)
                           False -> head ss : "." ++ (let bt = (backtrim . tail) ss in
                                                     if null bt then "0" 
                                                       else bt )
                                                  ++ "e" ++ show (pred e)
                    where (str, e) = dyadicToString Near n 10 d
                          n        = max dec 5
                          (s, ss) = case head str of
                                      '-' -> ("-", tail str)
                                      _   -> ("" , str)
                          backtrim = reverse . dropWhile (== '0') . reverse 

toString       :: Word -> Dyadic -> String
toString dec d = s ++ case compare 0 e of
                         LT -> take e ss ++ (let bt = all (== '0') (drop e ss) in if bt then "" else '.' : (drop e ss))
                               ++ (if fromIntegral n - e < 0 then "e" ++ show (e - fromIntegral n) else "")
                         GT -> let ee = fromIntegral dec + e in 
                               if ee <= 0 then "0" else 
                                   head ss : "." ++ (backtrim . tail . take ee) ss ++ "e" ++ show (pred e)
                         EQ -> "0." ++ let bt = all (== '0') ss in if bt then "0" else ss
                  where (str, e) = dyadicToString Near n 10 d
                        n        = max dec 5
                        (s, ss) = case head str of
                                    '-' -> ("-", tail str)
                                    _   -> ("" , str)
                        backtrim = reverse . dropWhile (== '0') . reverse 

 
------------------------------------
-- mpfr constants

----------------------------------------------------------
-- conversion from basic haskell types to dyadics


fromIntegerA       :: RoundMode -> Precision -> Integer -> Dyadic
fromIntegerA r p d = stringToDyadic r p 10 (show d)

compose             :: RoundMode -> Precision -> (Integer, Int) -> Dyadic 
compose r p (i, ii) = div2i r p (fromIntegerA r p i) ii

fromString       :: String -> Precision -> Word -> Dyadic
fromString s p b = stringToDyadic Near p b s

---------------------------------------------------------

-- functions getting properties of dyadics

getPrec   :: Dyadic -> Precision
getPrec d = fromIntegral (withDyadicP d mpfr_get_prec)

-- | getMantissa and getExp return values such that
--
-- > d = getMantissa d * 2^(getExp d - Prelude.ceiling ((getPrec d) / bitsPerMPLimb)* bitsPerMPLimb )
getMantissa   :: Dyadic -> Integer
getMantissa d = if d < zero then -h else h
               where (h, _) = foldl (\(a,b) c -> (a + (toInteger c) `shiftL` b, b + bitsPerMPLimb)) (0,0) (getMantissa' d) 


--------------------------------------------------------

-- some constants
minPrec :: Precision
minPrec = 32

one ::  Dyadic              
one = fromWord Near minPrec 1

zero :: Dyadic              
zero = fromWord Near minPrec 0

-- instances

instance Eq Dyadic where
    (==) = equal

instance Ord Dyadic where
    (<)  = less
    (<=) = lesseq
    (>)  = greater
    (>=) = greatereq
                     
instance Show Dyadic where
    show = toStringExp 16

-- these are exact operations, without rounding
instance Num Dyadic where
    d + d' = add Zero (addPrec d d') d d'
    d - d' = sub Zero (addPrec d d') d d'
    d * d' = mul Zero (getPrec d + getPrec d') d d'
    negate d = neg Zero (getPrec d) d 
    signum d = case compare d zero of 
                 LT -> negate one
                 EQ -> zero
                 _  -> one
    abs d = absD Zero (getPrec d) d
    fromInteger i = fromIntegerA Zero (checkPrec $ binprec i) i

addPrec       :: Dyadic -> Dyadic -> Precision
addPrec d1 d2 = fromIntegral (max (p1 + e1 - e3) (p2 + e2 - e3)) + 1
                where e1 = if d1 == 0 then 0 else getExp d1
                      e2 = if d2 == 0 then 0 else getExp d2
                      p1 = fromIntegral $ getPrec d1
                      p2 = fromIntegral $ getPrec d2
                      e3 = min e1 e2

{-
addPrec d1 d2 = max e1 e2 + 1 - min (e1 - p1) (p2 - e2)
                where e1 = if d1 == 0 then 0 else fromIntegral $ getExp d1
                      e2 = if d2 == 0 then 0 else fromIntegral $ getExp d2
                      p1 = getPrec d1
                      p2 = getPrec d2
-}


