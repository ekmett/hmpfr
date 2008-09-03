{-# LANGUAGE ForeignFunctionInterface #-}
#include <helper.h>
#include <mpfr.h>

module Data.Number.FFIhelper where

import Foreign.C.String(CString)
import Foreign.C.Types(CULong, CLong, CInt, CUInt, CDouble)
import Foreign.Ptr(FunPtr, Ptr)

data RoundMode = Near | Zero | Up | Down | GMP_RND_MAX | GMP_RNDNA 

instance Enum RoundMode where
    fromEnum Near        = #{const GMP_RNDN} 
    fromEnum Zero        = #{const GMP_RNDZ} 
    fromEnum Up          = #{const GMP_RNDU} 
    fromEnum Down        = #{const GMP_RNDD} 
    fromEnum GMP_RND_MAX = #{const GMP_RND_MAX}
    fromEnum GMP_RNDNA    = #{const GMP_RNDNA}
    
    toEnum #{const GMP_RNDN}    = Near
    toEnum #{const GMP_RNDZ}    = Zero
    toEnum #{const GMP_RNDU}    = Up
    toEnum #{const GMP_RNDD}    = Down
    toEnum #{const GMP_RND_MAX} = GMP_RND_MAX
    toEnum (#{const GMP_RNDNA}) = GMP_RNDNA
    toEnum i                    = error $ "RoundMode.toEnum called with illegal argument :" ++ show i 

bitsPerMPLimb :: Int 
bitsPerMPLimb = 8 * #size mp_limb_t

type CRoundMode = CInt

type CPrecision = CUInt

data MPFR_T = MPFR_T

-- utility functions from helper.h
foreign import ccall unsafe "initS"
        initS :: CPrecision -> IO (Ptr MPFR_T)

foreign import ccall unsafe "&clear"
        clear :: FunPtr (Ptr MPFR_T -> IO ())

--------------------
foreign import ccall unsafe "mpfr_get_prec_wrap"
        mpfr_get_prec :: Ptr MPFR_T -> IO CPrecision 

-- assignment functions
foreign import ccall unsafe "mpfr_set_wrap"
        mpfr_set :: Ptr MPFR_T -> Ptr MPFR_T -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_set_ui_wrap"
        mpfr_set_ui :: Ptr MPFR_T -> CULong -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_set_si_wrap"
        mpfr_set_si :: Ptr MPFR_T -> CLong -> CRoundMode -> IO CInt

--TODO set_uj, set_sj

foreign import ccall unsafe "mpfr_set_d"
        mpfr_set_d :: Ptr MPFR_T -> CDouble -> CRoundMode -> IO CInt

--foreign import ccall unsafe "mfpr_set_ld"
  --      mpfr_set_ld :: Ptr MPFR_T -> #{type long double} -> CRoundMode -> IO CInt
--long double does not seem to be supported

--TODO figure out how to set from Integer

foreign import ccall unsafe "mpfr_set_ui_2exp"
        mpfr_set_ui_2exp :: Ptr MPFR_T -> CULong -> CInt -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_set_si_2exp"
        mpfr_set_si_2exp :: Ptr MPFR_T -> CLong -> CInt -> CRoundMode -> IO CInt

--TODO set_uj_2exp, set_sj_2exp

foreign import ccall unsafe "mpfr_set_str"
        mpfr_set_str :: Ptr MPFR_T -> CString -> CInt -> CRoundMode -> IO CInt

--TODO strtofr, set_inf, set_nan, swap

-- THINK combined initialization and assignment functions are non-applicable with custom interface?

---------------------------------------------------------------------------------

-- conversion functions
foreign import ccall unsafe "mpfr_get_d"
        mpfr_get_d :: Ptr MPFR_T -> CRoundMode -> IO CDouble

--get_d_2exp, get_ld_2exp

foreign import ccall unsafe "mpfr_get_si" 
        mpfr_get_si :: Ptr MPFR_T -> CRoundMode -> IO CLong

foreign import ccall unsafe "mpfr_get_ui" 
        mpfr_get_ui :: Ptr MPFR_T -> CRoundMode -> IO CULong

--TODO get_z_exp, get_z, get_f, 

foreign import ccall unsafe "mpfr_get_str"
        mpfr_get_str :: CString -> Ptr CInt -> CInt -> CUInt -> Ptr MPFR_T ->  CRoundMode -> IO CString

--TODO all the fits* functions

---------------------------------------------------------------------------------

-- basic arithmetic functions

foreign import ccall unsafe "mpfr_add"
        mpfr_add :: Ptr MPFR_T -> Ptr MPFR_T -> Ptr MPFR_T -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_add_ui"
        mpfr_add_ui :: Ptr MPFR_T -> Ptr MPFR_T -> CULong -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_add_si"
        mpfr_add_si :: Ptr MPFR_T -> Ptr MPFR_T -> CLong -> CRoundMode -> IO CInt

-- TODO add_z, add_q

foreign import ccall unsafe "mpfr_sub"
        mpfr_sub :: Ptr MPFR_T -> Ptr MPFR_T -> Ptr MPFR_T -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_ui_sub" 
        mpfr_ui_sub :: Ptr MPFR_T -> CULong -> Ptr MPFR_T -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_sub_ui"
        mpfr_sub_ui :: Ptr MPFR_T -> Ptr MPFR_T -> CULong -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_si_sub" 
        mpfr_si_sub :: Ptr MPFR_T -> CLong -> Ptr MPFR_T -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_sub_si"
        mpfr_sub_si :: Ptr MPFR_T -> Ptr MPFR_T -> CLong -> CRoundMode -> IO CInt

--TODO sub_z, sub_q

foreign import ccall unsafe "mpfr_mul"
        mpfr_mul :: Ptr MPFR_T -> Ptr MPFR_T -> Ptr MPFR_T -> CRoundMode -> IO CInt 

foreign import ccall unsafe "mpfr_mul_ui"
        mpfr_mul_ui :: Ptr MPFR_T -> Ptr MPFR_T -> CULong -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_mul_si"
        mpfr_mul_si :: Ptr MPFR_T -> Ptr MPFR_T -> CLong -> CRoundMode -> IO CInt

--TODO mul_z, mul_q

foreign import ccall unsafe "mpfr_sqr"
        mpfr_sqr :: Ptr MPFR_T -> Ptr MPFR_T -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_div"
        mpfr_div :: Ptr MPFR_T -> Ptr MPFR_T -> Ptr MPFR_T -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_ui_div"
        mpfr_ui_div :: Ptr MPFR_T -> CULong -> Ptr MPFR_T -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_div_ui"
        mpfr_div_ui :: Ptr MPFR_T -> Ptr MPFR_T -> CULong -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_si_div"
        mpfr_si_div :: Ptr MPFR_T -> CLong -> Ptr MPFR_T -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_div_si"
        mpfr_div_si :: Ptr MPFR_T -> Ptr MPFR_T -> CLong -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_sqrt"
        mpfr_sqrt :: Ptr MPFR_T -> Ptr MPFR_T -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_sqrt_ui"
        mpfr_sqrt_ui :: Ptr MPFR_T -> CULong -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_cbrt"
        mpfr_cbrt :: Ptr MPFR_T -> Ptr MPFR_T -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_root"
        mpfr_root :: Ptr MPFR_T -> Ptr MPFR_T -> CULong -> CRoundMode -> IO CInt 

foreign import ccall unsafe "mpfr_pow"
        mpfr_pow :: Ptr MPFR_T -> Ptr MPFR_T -> Ptr MPFR_T -> CRoundMode -> IO CInt 

foreign import ccall unsafe "mpfr_pow_ui"
        mpfr_pow_ui :: Ptr MPFR_T -> Ptr MPFR_T -> CULong -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_pow_si"
        mpfr_pow_si :: Ptr MPFR_T -> Ptr MPFR_T -> CLong -> CRoundMode -> IO CInt

--TODO pow_z

foreign import ccall unsafe "mpfr_ui_pow_ui"
        mpfr_ui_pow_ui :: Ptr MPFR_T -> CULong -> CULong -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_ui_pow"
        mpfr_ui_pow :: Ptr MPFR_T -> CULong -> Ptr MPFR_T -> CRoundMode -> IO CInt


foreign import ccall unsafe "mpfr_neg"
        mpfr_neg :: Ptr MPFR_T -> Ptr MPFR_T -> CRoundMode -> IO CInt 

foreign import ccall unsafe "mpfr_abs_wrap"
        mpfr_abs :: Ptr MPFR_T -> Ptr MPFR_T -> CRoundMode -> IO CInt 

foreign import ccall unsafe "mpfr_dim"
        mpfr_dim :: Ptr MPFR_T -> Ptr MPFR_T -> Ptr MPFR_T -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_mul_2ui"
        mpfr_mul_2ui :: Ptr MPFR_T -> Ptr MPFR_T -> CULong -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_mul_2si"
        mpfr_mul_2si :: Ptr MPFR_T -> Ptr MPFR_T -> CLong -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_div_2ui"
        mpfr_div_2ui :: Ptr MPFR_T -> Ptr MPFR_T -> CULong -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_div_2si"
        mpfr_div_2si :: Ptr MPFR_T -> Ptr MPFR_T -> CLong -> CRoundMode -> IO CInt

---------------------------------------------------------------------------------
-- comparison functions
{- 
foreign import ccall unsafe "mpfr_cmp"
        mpfr_cmp :: Ptr MPFR_T -> Ptr MPFR_T -> IO CInt

--foreign import ccall unsafe "mpfr_cmp_ui"
--        mpfr_cmp_ui :: Ptr MPFR_T -> CULong -> IO CInt

--foreign import ccall unsafe "mpfr_cmp_si"
--        mpfr_cmp_si :: Ptr MPFR_T -> CLong -> IO CInt

foreign import ccall unsafe "mpfr_cmp_d"
        mpfr_cmp_d :: Ptr MPFR_T -> CDouble -> IO CInt

--TODO cmp_ld, cmp_z, cmp_q, cmp_f

foreign import ccall unsafe "mpfr_cmp_ui_2exp"
        mpfr_cmp_ui_2exp :: Ptr MPFR_T -> CULong -> CInt -> IO CInt

foreign import ccall unsafe "mpfr_cmp_si_2exp"
        mpfr_cmp_si_2exp :: Ptr MPFR_T -> CLong -> CInt -> IO CInt

foreign import ccall unsafe "mpfr_cmpabs"
        mpfr_cmpabs :: Ptr MPFR_T -> Ptr MPFR_T -> IO CInt
-}
foreign import ccall unsafe "mpfr_nan_p_wrap"
        mpfr_nan_p :: Ptr MPFR_T -> IO CInt

foreign import ccall unsafe "mpfr_inf_p_wrap"
        mpfr_inf_p :: Ptr MPFR_T -> IO CInt

foreign import ccall unsafe "mpfr_number_p"
        mpfr_number_p :: Ptr MPFR_T -> IO CInt

foreign import ccall unsafe "mpfr_zero_p_wrap"
        mpfr_zero_p :: Ptr MPFR_T -> IO CInt

-- sets erange flag
--foreign import ccall unsafe "mpfr_sgn"
--        mpfr_sgn :: Ptr MPFR_T -> IO CInt 

foreign import ccall unsafe "mpfr_greater_p"
        mpfr_greater_p :: Ptr MPFR_T ->  Ptr MPFR_T -> IO CInt

foreign import ccall unsafe "mpfr_greaterequal_p"
        mpfr_greaterequal_p :: Ptr MPFR_T -> Ptr MPFR_T -> IO CInt 

foreign import ccall unsafe "mpfr_less_p"
        mpfr_less_p :: Ptr MPFR_T -> Ptr MPFR_T -> IO CInt 

foreign import ccall unsafe "mpfr_lessequal_p"
        mpfr_lessequal_p :: Ptr MPFR_T -> Ptr MPFR_T -> IO CInt 

foreign import ccall unsafe "mpfr_lessgreater_p"
        mpfr_lessgreater_p :: Ptr MPFR_T -> Ptr MPFR_T -> IO CInt 

foreign import ccall unsafe "mpfr_equal_p"
        mpfr_equal_p :: Ptr MPFR_T -> Ptr MPFR_T -> IO CInt 

foreign import ccall unsafe "mpfr_unordered_p"
        mpfr_unordered_p :: Ptr MPFR_T -> Ptr MPFR_T -> IO CInt 

-- integer related functions

foreign import ccall unsafe "mpfr_rint"
        mpfr_rint :: Ptr MPFR_T -> Ptr MPFR_T -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_ceil_wrap"
        mpfr_ceil :: Ptr MPFR_T -> Ptr MPFR_T  -> IO CInt

foreign import ccall unsafe "mpfr_floor_wrap"
        mpfr_floor :: Ptr MPFR_T -> Ptr MPFR_T -> IO CInt

foreign import ccall unsafe "mpfr_round_wrap"
        mpfr_round :: Ptr MPFR_T -> Ptr MPFR_T -> IO CInt

foreign import ccall unsafe "mpfr_trunc_wrap"
        mpfr_trunc :: Ptr MPFR_T -> Ptr MPFR_T -> IO CInt
 
--------------------
-- miscellaneus functions
foreign import ccall unsafe "mpfr_min"
        mpfr_min :: Ptr MPFR_T -> Ptr MPFR_T -> Ptr MPFR_T -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_max"
        mpfr_max :: Ptr MPFR_T -> Ptr MPFR_T -> Ptr MPFR_T -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_fma"
        mpfr_fma :: Ptr MPFR_T -> Ptr MPFR_T -> Ptr MPFR_T -> Ptr MPFR_T -> CRoundMode -> IO CInt  

foreign import ccall unsafe "mpfr_fms"
        mpfr_fms :: Ptr MPFR_T -> Ptr MPFR_T -> Ptr MPFR_T -> Ptr MPFR_T -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_nextbelow"
        mpfr_nextbelow ::  Ptr MPFR_T -> IO ()

foreign import ccall unsafe "mpfr_sinh"
        mpfr_sinh :: Ptr MPFR_T -> Ptr MPFR_T ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_cosh"
        mpfr_cosh :: Ptr MPFR_T -> Ptr MPFR_T ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_tanh"
        mpfr_tanh :: Ptr MPFR_T -> Ptr MPFR_T ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_exp"
        mpfr_exp :: Ptr MPFR_T -> Ptr MPFR_T ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_exp2"
        mpfr_exp2 :: Ptr MPFR_T -> Ptr MPFR_T ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_exp10"
        mpfr_exp10 :: Ptr MPFR_T -> Ptr MPFR_T ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_log"
        mpfr_log :: Ptr MPFR_T -> Ptr MPFR_T ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_log2"
        mpfr_log2 :: Ptr MPFR_T -> Ptr MPFR_T ->  CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_log10"
        mpfr_log10 :: Ptr MPFR_T -> Ptr MPFR_T ->  CRoundMode -> IO CInt

-------------------------------------------------
-- custom access functions
foreign import ccall unsafe "mpfr_custom_get_mantissa_wrap"
        mpfr_custom_get_mantissa :: Ptr MPFR_T -> IO (Ptr CULong)

foreign import ccall unsafe "mpfr_custom_get_exp_wrap"
        mpfr_custom_get_exp :: Ptr MPFR_T -> IO CInt

-------------------------------------------------
-- constants
foreign import ccall unsafe "mpfr_const_pi"
        mpfr_const_pi :: Ptr MPFR_T -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_const_log2"
        mpfr_const_log2 :: Ptr MPFR_T -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_const_euler"
        mpfr_const_euler :: Ptr MPFR_T -> CRoundMode -> IO CInt

foreign import ccall unsafe "mpfr_const_catalan"
        mpfr_const_catalan :: Ptr MPFR_T -> CRoundMode -> IO CInt