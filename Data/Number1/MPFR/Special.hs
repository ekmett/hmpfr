{-# INCLUDE <mpfr.h> #-}
{-# INCLUDE <chsmpfr.h> #-}

module Data.Number1.MPFR.Special where

import Data.Number1.MPFR.Internal

log       :: RoundMode -> Precision -> MPFR -> MPFR
log r p d = fst $ log_ r p d

log2       :: RoundMode -> Precision -> MPFR -> MPFR
log2 r p d = fst $ log2_ r p d

log10       :: RoundMode -> Precision -> MPFR -> MPFR
log10 r p d = fst $ log10_ r p d

exp       :: RoundMode -> Precision -> MPFR -> MPFR
exp r p d = fst $ exp_ r p d

exp2       :: RoundMode -> Precision -> MPFR -> MPFR
exp2 r p d = fst $ exp2_ r p d

exp10       :: RoundMode -> Precision -> MPFR -> MPFR
exp10 r p d = fst $ exp10_ r p d

sin       :: RoundMode -> Precision -> MPFR -> MPFR
sin r p d = fst $ sin_ r p d

cos       :: RoundMode -> Precision -> MPFR -> MPFR
cos r p d = fst $ cos_ r p d

tan       :: RoundMode -> Precision -> MPFR -> MPFR
tan r p d = fst $ tan_ r p d 

sec       :: RoundMode -> Precision -> MPFR -> MPFR
sec r p d = fst $ sec_ r p d

csc       :: RoundMode -> Precision -> MPFR -> MPFR
csc r p d = fst $ csc_ r p d

cot       :: RoundMode -> Precision -> MPFR -> MPFR
cot r p d = fst $ cot_ r p d 

sincos :: RoundMode
       -> Precision -- ^ precision to compute sin
       -> Precision -- ^ precision to compute cos 
       -> MPFR
       -> (MPFR, MPFR) -- ^ return (sin x, cos x)
sincos r p p' d = case sincos_ r p p' d of
                    (a, b, _) -> (a, b)

asin       :: RoundMode -> Precision -> MPFR -> MPFR
asin r p d = fst $ asin_ r p d

acos       :: RoundMode -> Precision -> MPFR -> MPFR
acos r p d = fst $ acos_ r p d

atan       :: RoundMode -> Precision -> MPFR -> MPFR
atan r p d = fst $ atan_ r p d 

atan2          :: RoundMode -> Precision -> MPFR -> MPFR -> MPFR
atan2 r p d d' = fst $ atan2_ r p d d'

sinh       :: RoundMode -> Precision -> MPFR -> MPFR
sinh r p d = fst $ sinh_ r p d

cosh       :: RoundMode -> Precision -> MPFR -> MPFR
cosh r p d = fst $ cosh_ r p d

tanh       :: RoundMode -> Precision -> MPFR -> MPFR
tanh r p d = fst $ tanh_ r p d 

sech       :: RoundMode -> Precision -> MPFR -> MPFR
sech r p d = fst $ sech_ r p d

csch       :: RoundMode -> Precision -> MPFR -> MPFR
csch r p d = fst $ csch_ r p d

coth       :: RoundMode -> Precision -> MPFR -> MPFR
coth r p d = fst $ coth_ r p d 

acosh       :: RoundMode -> Precision -> MPFR -> MPFR
acosh r p d = fst $ acosh_ r p d

asinh       :: RoundMode -> Precision -> MPFR -> MPFR
asinh r p d = fst $ asinh_ r p d

atanh       :: RoundMode -> Precision -> MPFR -> MPFR
atanh r p d = fst $ atanh_ r p d 

facw       :: RoundMode -> Precision -> Word -> MPFR
facw r p d = fst $ facw_ r p d

log1p       :: RoundMode -> Precision -> MPFR -> MPFR
log1p r p d = fst $ log1p_ r p d

expm1       :: RoundMode -> Precision -> MPFR -> MPFR
expm1 r p d = fst $ expm1_ r p d

eint       :: RoundMode -> Precision -> MPFR -> MPFR
eint r p d = fst $ eint_ r p d

gamma       :: RoundMode -> Precision -> MPFR -> MPFR
gamma r p d = fst $ gamma_ r p d

lngamma       :: RoundMode -> Precision -> MPFR -> MPFR
lngamma r p d = fst $ lngamma_ r p d

lgamma       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
lgamma r p d = case lgamma_ r p d of 
                 (a, b, _) -> (a,b)

zeta       :: RoundMode -> Precision -> MPFR -> MPFR
zeta r p d = fst $ zeta_ r p d

zetaw       :: RoundMode -> Precision -> Word -> MPFR
zetaw r p d = fst $ zetaw_ r p d

erf       :: RoundMode -> Precision -> MPFR -> MPFR
erf r p d = fst $ erf_ r p d

erfc       :: RoundMode -> Precision -> MPFR -> MPFR
erfc r p d = fst $ erfc_ r p d

j0       :: RoundMode -> Precision -> MPFR -> MPFR
j0 r p d = fst $ j0_ r p d

j1       :: RoundMode -> Precision -> MPFR -> MPFR
j1 r p d = fst $ j1_ r p d

jn         :: RoundMode -> Precision -> Int -> MPFR -> MPFR
jn r p w d = fst $ jn_ r p w d

y0       :: RoundMode -> Precision -> MPFR -> MPFR
y0 r p d = fst $ y0_ r p d

y1       :: RoundMode -> Precision -> MPFR -> MPFR
y1 r p d = fst $ y1_ r p d

yn         :: RoundMode -> Precision -> Int -> MPFR -> MPFR
yn r p w d = fst $ yn_ r p w d

fma              :: RoundMode -> Precision -> MPFR -> MPFR -> MPFR -> MPFR
fma r p d1 d2 d3 = fst $ fma_ r p d1 d2 d3

fms              :: RoundMode -> Precision -> MPFR -> MPFR -> MPFR -> MPFR
fms r p d1 d2 d3 = fst $ fms_ r p d1 d2 d3

agm           :: RoundMode -> Precision -> MPFR -> MPFR -> MPFR
agm r p d1 d2 = fst $ agm_ r p d1 d2

hypot           :: RoundMode -> Precision -> MPFR -> MPFR -> MPFR
hypot r p d1 d2 = fst $ hypot_ r p d1 d2

pi :: RoundMode -> Precision -> MPFR
pi r p = fst $ pi_ r p

log2c     :: RoundMode -> Precision -> MPFR
log2c r p = fst $ pi_ r p

euler     :: RoundMode -> Precision -> MPFR
euler r p = fst $ pi_ r p

catalan     :: RoundMode -> Precision -> MPFR
catalan r p = fst $ pi_ r p


log_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
log_ r p d = withMPFR r p d mpfr_log

log2_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
log2_ r p d = withMPFR r p d mpfr_log2

log10_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
log10_ r p d = withMPFR r p d mpfr_log10

exp_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
exp_ r p d = withMPFR r p d mpfr_exp

exp2_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
exp2_ r p d = withMPFR r p d mpfr_exp2

exp10_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
exp10_ r p d = withMPFR r p d mpfr_exp10

sin_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
sin_ r p d = withMPFR r p d mpfr_sin

cos_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
cos_ r p d = withMPFR r p d mpfr_cos

tan_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
tan_ r p d = withMPFR r p d mpfr_tan

sec_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
sec_ r p d = withMPFR r p d mpfr_sec

csc_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
csc_ r p d = withMPFR r p d mpfr_csc

cot_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
cot_ r p d = withMPFR r p d mpfr_cot


sincos_ :: RoundMode
         -> Precision -- ^ precision to compute sin
         -> Precision -- ^ precision to compute cos 
         -> MPFR
         -> (MPFR, MPFR, Int)
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

asin_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
asin_ r p d = withMPFR r p d mpfr_asin

acos_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
acos_ r p d = withMPFR r p d mpfr_acos

atan_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
atan_ r p d = withMPFR r p d mpfr_atan

atan2_ :: RoundMode -> Precision -> MPFR -> MPFR -> (MPFR, Int)
atan2_ r p d d' = withMPFRsBA r p d d' mpfr_atan2 

sinh_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
sinh_ r p d = withMPFR r p d mpfr_sinh

cosh_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
cosh_ r p d = withMPFR r p d mpfr_cosh

tanh_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
tanh_ r p d = withMPFR r p d mpfr_tanh

sech_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
sech_ r p d = withMPFR r p d mpfr_sech

csch_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
csch_ r p d = withMPFR r p d mpfr_csch

coth_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
coth_ r p d = withMPFR r p d mpfr_coth

acosh_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
acosh_ r p d = withMPFR r p d mpfr_acosh

asinh_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
asinh_ r p d = withMPFR r p d mpfr_asinh

atanh_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
atanh_ r p d = withMPFR r p d mpfr_atanh

facw_       :: RoundMode -> Precision -> Word -> (MPFR, Int)
facw_ r p w = withMPFRUI r p w mpfr_fac_ui

log1p_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
log1p_ r p d = withMPFR r p d mpfr_log1p

expm1_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
expm1_ r p d = withMPFR r p d mpfr_expm1

eint_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
eint_ r p d = withMPFR r p d mpfr_eint

gamma_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
gamma_ r p d = withMPFR r p d mpfr_gamma

lngamma_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
lngamma_ r p d = withMPFR r p d mpfr_lngamma

lgamma_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int, Int)
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
                    
zeta_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
zeta_ r p d = withMPFR r p d mpfr_zeta

zetaw_       :: RoundMode -> Precision -> Word -> (MPFR, Int)
zetaw_ r p d = withMPFRUI r p d mpfr_zeta_ui

erf_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
erf_ r p d = withMPFR r p d mpfr_erf

erfc_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
erfc_ r p d = withMPFR r p d mpfr_erfc

j0_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
j0_ r p d = withMPFR r p d mpfr_j0

j1_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
j1_ r p d = withMPFR r p d mpfr_j1

jn_         :: RoundMode -> Precision -> Int -> MPFR -> (MPFR, Int)
jn_ r p i d = withMPFRBAis r p (fromIntegral i) d mpfr_jn

y0_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
y0_ r p d = withMPFR r p d mpfr_y0

y1_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
y1_ r p d = withMPFR r p d mpfr_y1

yn_         :: RoundMode -> Precision -> Int -> MPFR -> (MPFR, Int)
yn_ r p i d = withMPFRBAis r p (fromIntegral i) d mpfr_yn

fma_                 :: RoundMode -> Precision -> MPFR -> MPFR -> MPFR -> (MPFR, Int)
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

fms_                 :: RoundMode -> Precision -> MPFR -> MPFR -> MPFR -> (MPFR, Int)
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

agm_           :: RoundMode -> Precision -> MPFR -> MPFR -> (MPFR,Int)
agm_ r p d1 d2 =  withMPFRsBA r p d1 d2 mpfr_agm

hypot_           :: RoundMode -> Precision -> MPFR -> MPFR -> (MPFR,Int)
hypot_ r p d1 d2 =  withMPFRsBA r p d1 d2 mpfr_hypot

pi_     :: RoundMode -> Precision -> (MPFR, Int)
pi_ r p = withMPFRC r p mpfr_const_pi

log2c_     :: RoundMode -> Precision -> (MPFR, Int)
log2c_ r p = withMPFRC r p mpfr_const_log2

euler_     :: RoundMode -> Precision -> (MPFR, Int)
euler_ r p = withMPFRC r p mpfr_const_euler

catalan_     :: RoundMode -> Precision -> (MPFR, Int)
catalan_ r p = withMPFRC r p mpfr_const_catalan

freeCache :: IO ()
freeCache = mpfr_free_cache
