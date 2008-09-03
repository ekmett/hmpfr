#include <mpfr.h>
#include <malloc.h>

mpfr_ptr initS(const mp_prec_t );

void clear (const mpfr_ptr ) ;

// these functions are defined as macros and so haskell ffi 
// can't work with them directly

int mpfr_nan_p_wrap(const mpfr_ptr) ;

int mpfr_inf_p_wrap(const mpfr_ptr) ;

int mpfr_zero_p_wrap(const mpfr_ptr) ;

int mpfr_set_wrap(const mpfr_ptr p1, const mpfr_ptr p2, mp_rnd_t r) ;

int mpfr_abs_wrap(const mpfr_ptr, const mpfr_ptr, mp_rnd_t) ;

int mpfr_set_si_wrap (const mpfr_ptr, long int, mp_rnd_t) ;

int mpfr_set_ui_wrap (const mpfr_ptr, unsigned long int, mp_rnd_t) ;

int mpfr_ceil_wrap (const mpfr_ptr , const mpfr_ptr ) ;

int mpfr_floor_wrap (const mpfr_ptr , const mpfr_ptr ) ;

int mpfr_round_wrap (const mpfr_ptr , const mpfr_ptr ) ;

int mpfr_trunc_wrap (const mpfr_ptr , const mpfr_ptr ) ;

mp_prec_t mpfr_get_prec_wrap (const mpfr_ptr ) ;

void * mpfr_custom_get_mantissa_wrap (const mpfr_ptr ) ;

mp_exp_t mpfr_custom_get_exp_wrap (const mpfr_ptr ) ;
