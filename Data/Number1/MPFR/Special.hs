{-# INCLUDE <mpfr.h> #-}
{-# INCLUDE <chsmpfr.h> #-}

module Data.Number1.MPFR.Special where

import Data.Number1.MPFR.Internal

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
sincos r p p' d = case sincos_ r p p' d of
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

yn         :: RoundMode -> Precision -> Int -> Dyadic -> Dyadic
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
         -> (Dyadic, Dyadic, Int)
sincos_ r p p' d = unsafePerformIO go 
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
atan2_ r p d d' = withDyadicsBA r p d d' mpfr_atan2 

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
