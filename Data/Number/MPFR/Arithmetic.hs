{-|
    Module      :  Data.Number.MPFR.Arithmetic
    Description :  wrappers for basic arithmetic functions
    Copyright   :  (c) Aleš Bizjak
    License     :  BSD3

    Maintainer  :  ales.bizjak0@gmail.com
    Stability   :  experimental
    Portability :  non-portable

 Basic arithmetic functions. See MPFR manual for detailed documentation.
-}
{-# INCLUDE <mpfr.h> #-}
{-# INCLUDE <chsmpfr.h> #-}

module Data.Number.MPFR.Arithmetic 
    where

import Data.Number.MPFR.Internal

add           :: RoundMode -> Precision -> MPFR -> MPFR -> MPFR
add r p d1 d2 = fst $ add_ r p d1 d2 
      
addw          :: RoundMode -> Precision -> MPFR -> Word -> MPFR
addw r p d1 d = fst $ addw_ r p d1 d 
      
addi          :: RoundMode -> Precision -> MPFR -> Int -> MPFR
addi r p d1 d = fst $ addi_ r p d1 d 
      
sub           :: RoundMode -> Precision -> MPFR -> MPFR -> MPFR
sub r p d1 d2 = fst $ sub_ r p d1 d2
      
subw          :: RoundMode -> Precision -> MPFR -> Word -> MPFR
subw r p d1 d = fst $ subw_ r p d1 d 
      
subi          :: RoundMode -> Precision -> MPFR -> Int -> MPFR
subi r p d1 d = fst $ subi_ r p d1 d 
      
wsub          :: RoundMode -> Precision -> Word -> MPFR -> MPFR
wsub r p d d1 = fst $ wsub_ r p d d1 
      
isub          :: RoundMode -> Precision -> Int -> MPFR -> MPFR
isub r p d d1 = fst $ isub_ r p d d1 
      
mul           :: RoundMode -> Precision -> MPFR -> MPFR -> MPFR
mul r p d1 d2 = fst $ mul_ r p d1 d2

mulw          :: RoundMode -> Precision -> MPFR -> Word -> MPFR
mulw r p d1 d = fst $ mulw_ r p d1 d 
      
muli          :: RoundMode -> Precision -> MPFR -> Int -> MPFR
muli r p d1 d = fst $ muli_ r p d1 d 
      
sqr       :: RoundMode -> Precision -> MPFR -> MPFR 
sqr r p d = fst $ sqr_ r p d
      
div           :: RoundMode -> Precision -> MPFR -> MPFR -> MPFR
div r p d1 d2 = fst $ div_ r p d1 d2
      
divw          :: RoundMode -> Precision -> MPFR -> Word -> MPFR
divw r p d1 d = fst $ divw_ r p d1 d 
      
divi          :: RoundMode -> Precision -> MPFR -> Int -> MPFR
divi r p d1 d = fst $ divi_ r p d1 d 
      
wdiv          :: RoundMode -> Precision -> Word -> MPFR -> MPFR
wdiv r p d d1 = fst $ wdiv_ r p d d1 
      
idiv          :: RoundMode -> Precision -> Int -> MPFR -> MPFR
idiv r p d d1 = fst $ idiv_ r p d d1 
      
sqrt       :: RoundMode -> Precision -> MPFR -> MPFR
sqrt r p d = fst $ sqrt_ r p d
      
sqrtw       :: RoundMode -> Precision -> Word -> MPFR
sqrtw r p d = fst $ sqrtw_ r p d
      
cbrt       :: RoundMode -> Precision -> MPFR -> MPFR
cbrt r p d = fst $ cbrt_ r p d
      
root         :: RoundMode -> Precision -> MPFR -> Word -> MPFR
root r p d n = fst $ root_ r p d n
      
pow           :: RoundMode -> Precision -> MPFR -> MPFR -> MPFR
pow r p d1 d2 = fst $ pow_ r p d1 d2 
      
poww           :: RoundMode -> Precision -> MPFR -> Word -> MPFR 
poww r p d1 d2 = fst $ poww_ r p d1 d2
      
powi           :: RoundMode -> Precision -> MPFR -> Int -> MPFR 
powi r p d1 d2 = fst $ powi_ r p d1 d2
      
wpoww           :: RoundMode -> Precision -> Word -> Word -> MPFR 
wpoww r p d1 d2 = fst $ wpoww_ r p d1 d2
      
wpow           :: RoundMode -> Precision -> Word -> MPFR -> MPFR 
wpow r p d1 d2 = fst $ wpow_ r p d1 d2
      
neg       :: RoundMode -> Precision -> MPFR -> MPFR
neg r p d = fst $ neg_ r p d
      
absD      :: RoundMode -> Precision -> MPFR -> MPFR 
absD r p d = fst $ absD_ r p d
      
dim           :: RoundMode -> Precision -> MPFR -> MPFR -> MPFR
dim r p d1 d2 = fst $ dim_ r p d1 d2 
      
mul2w           :: RoundMode -> Precision -> MPFR -> Word -> MPFR
mul2w r p d1 d2 = fst $ mul2w_ r p d1 d2
      
mul2i          :: RoundMode -> Precision -> MPFR -> Int -> MPFR
mul2i r p d1 d2 = fst $ mul2i_ r p d1 d2
      
div2w          :: RoundMode -> Precision -> MPFR -> Word -> MPFR
div2w r p d1 d2 = fst $ div2w_ r p d1 d2
      
div2i          :: RoundMode -> Precision -> MPFR -> Int -> MPFR
div2i r p d1 d2 = fst $ div2i_ r p d1 d2
      
add_           :: RoundMode -> Precision -> MPFR -> MPFR -> (MPFR,Int)
add_ r p d1 d2 =  withMPFRsBA r p d1 d2 mpfr_add
      
addw_          :: RoundMode -> Precision -> MPFR -> Word -> (MPFR, Int)
addw_ r p d1 d = withMPFRBAui r p d1 (fromIntegral d) mpfr_add_ui
      
addi_          :: RoundMode -> Precision -> MPFR -> Int -> (MPFR, Int)
addi_ r p d1 d = withMPFRBAsi r p d1 (fromIntegral d) mpfr_add_si
      
sub_           :: RoundMode -> Precision -> MPFR -> MPFR -> (MPFR,Int)
sub_ r p d1 d2 =  withMPFRsBA r p d1 d2 mpfr_sub
      
subw_          :: RoundMode -> Precision -> MPFR -> Word -> (MPFR, Int)
subw_ r p d1 d = withMPFRBAui r p d1 (fromIntegral d) mpfr_sub_ui
      
subi_          :: RoundMode -> Precision -> MPFR -> Int -> (MPFR, Int)
subi_ r p d1 d = withMPFRBAsi r p d1 (fromIntegral d) mpfr_sub_si
      
wsub_          :: RoundMode -> Precision -> Word -> MPFR -> (MPFR, Int)
wsub_ r p d d1 = withMPFRBAiu r p (fromIntegral d) d1 mpfr_ui_sub
      
isub_          :: RoundMode -> Precision -> Int -> MPFR -> (MPFR, Int)
isub_ r p d d1 = withMPFRBAis r p (fromIntegral d) d1 mpfr_si_sub
      
mul_           :: RoundMode -> Precision -> MPFR -> MPFR -> (MPFR,Int)
mul_ r p d1 d2 =  withMPFRsBA r p d1 d2 mpfr_mul
      
mulw_          :: RoundMode -> Precision -> MPFR -> Word -> (MPFR, Int)
mulw_ r p d1 d = withMPFRBAui r p d1 (fromIntegral d) mpfr_mul_ui
      
muli_          :: RoundMode -> Precision -> MPFR -> Int -> (MPFR, Int)
muli_ r p d1 d = withMPFRBAsi r p d1 (fromIntegral d) mpfr_mul_si
      
sqr_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
sqr_ r p d = withMPFR r p d mpfr_sqr
      
div_           :: RoundMode -> Precision -> MPFR -> MPFR -> (MPFR, Int)
div_ r p d1 d2 =  withMPFRsBA r p d1 d2 mpfr_div
      
divw_          :: RoundMode -> Precision -> MPFR -> Word -> (MPFR, Int)
divw_ r p d1 d = withMPFRBAui r p d1 (fromIntegral d) mpfr_div_ui
      
divi_          :: RoundMode -> Precision -> MPFR -> Int -> (MPFR, Int)
divi_ r p d1 d = withMPFRBAsi r p d1 (fromIntegral d) mpfr_div_si
      
wdiv_          :: RoundMode -> Precision -> Word -> MPFR -> (MPFR, Int)
wdiv_ r p d d1 = withMPFRBAiu r p (fromIntegral d) d1 mpfr_ui_div
      
idiv_          :: RoundMode -> Precision -> Int -> MPFR -> (MPFR, Int)
idiv_ r p d d1 = withMPFRBAis r p (fromIntegral d) d1 mpfr_si_div
      
sqrt_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
sqrt_ r p d = withMPFR r p d mpfr_sqrt
      
sqrtw_       :: RoundMode -> Precision -> Word -> (MPFR, Int)
sqrtw_ r p d = withMPFRUI r p d mpfr_sqrt_ui
      
cbrt_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
cbrt_ r p d = withMPFR r p d mpfr_cbrt
      
root_        :: RoundMode -> Precision -> MPFR -> Word -> (MPFR, Int)
root_ r p d n = withMPFRBAui r p d (fromIntegral n) mpfr_root
      
pow_          :: RoundMode -> Precision -> MPFR -> MPFR -> (MPFR, Int)
pow_ r p d1 d2 = withMPFRsBA r p d1 d2 mpfr_pow 
      
poww_          :: RoundMode -> Precision -> MPFR -> Word -> (MPFR , Int)
poww_ r p d1 d2 = withMPFRBAui r p d1 (fromIntegral d2) mpfr_pow_ui
      
powi_           :: RoundMode -> Precision -> MPFR -> Int -> (MPFR , Int)
powi_ r p d1 d2 = withMPFRBAsi r p d1 (fromIntegral d2) mpfr_pow_si
      
wpoww_          :: RoundMode -> Precision -> Word -> Word -> (MPFR , Int)
wpoww_ r p d1 d2 = unsafePerformIO go
    where go = do ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  let dummy = MP (fromIntegral p) 0 0 fp
                  with dummy $ \p1 -> do 
                    r2 <- mpfr_ui_pow_ui p1 (fromIntegral d1) (fromIntegral d2) ((fromIntegral . fromEnum) r)
                    r1 <- peekP p1 fp
                    return (r1, fromIntegral r2)
        
wpow_           :: RoundMode -> Precision -> Word -> MPFR -> (MPFR , Int)
wpow_ r p d1 d2 = withMPFRBAiu r p (fromIntegral d1) d2 mpfr_ui_pow
      
neg_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
neg_ r p d = withMPFR r p d mpfr_neg
      
absD_      :: RoundMode -> Precision -> MPFR -> (MPFR , Int)
absD_ r p d = withMPFR r p d mpfr_abs
      
dim_           :: RoundMode -> Precision -> MPFR -> MPFR -> (MPFR, Int)
dim_ r p d1 d2 = withMPFRsBA r p d1 d2 mpfr_dim
      
mul2w_           :: RoundMode -> Precision -> MPFR -> Word -> (MPFR, Int)
mul2w_ r p d1 d2 = withMPFRBAui r p d1 (fromIntegral d2) mpfr_mul_2ui
      
mul2i_          :: RoundMode -> Precision -> MPFR -> Int -> (MPFR, Int)
mul2i_ r p d1 d2 = withMPFRBAsi r p d1 (fromIntegral d2) mpfr_mul_2si
      
div2w_          :: RoundMode -> Precision -> MPFR -> Word -> (MPFR, Int)
div2w_ r p d1 d2 = withMPFRBAui r p d1 (fromIntegral d2) mpfr_div_2ui
      
div2i_          :: RoundMode -> Precision -> MPFR -> Int -> (MPFR, Int)
div2i_ r p d1 d2 = withMPFRBAsi r p d1 (fromIntegral d2) mpfr_div_2si
