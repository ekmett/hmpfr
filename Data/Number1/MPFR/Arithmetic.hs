{-# INCLUDE <mpfr.h> #-}
{-# INCLUDE <chsmpfr.h> #-}

module Data.Number1.MPFR.Arithmetic where

import Data.Number1.MPFR.Internal

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
