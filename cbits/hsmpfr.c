#include "helper.h"


mpfr_ptr initS (const mp_prec_t prec ) {
  mpfr_ptr rVal = malloc (sizeof(__mpfr_struct));
  mp_limb_t *limb = (mp_limb_t*)malloc (mpfr_custom_get_size(prec) * sizeof(mp_limb_t));
  mpfr_custom_init(limb, prec);
  mpfr_custom_init_set(rVal, MPFR_NAN_KIND, 0, prec, limb);
  return rVal;
}

void clear (const mpfr_ptr p) {
  free (p->_mpfr_d);
  free (p);
}

int mpfr_nan_p_wrap(const mpfr_ptr p) {
  return mpfr_nan_p(p);
}

int mpfr_inf_p_wrap(const mpfr_ptr p) {
  return mpfr_inf_p(p);
}
int mpfr_zero_p_wrap(const mpfr_ptr p) {
  return mpfr_inf_p(p); 
}

int mpfr_set_wrap(const mpfr_ptr p1, const mpfr_ptr p2, mp_rnd_t r) {
  return mpfr_set(p1, p2, r);
}

int mpfr_abs_wrap(const mpfr_ptr p1, const mpfr_ptr p2, mp_rnd_t r) {
  return mpfr_abs(p1, p2, r);
}

int mpfr_cmp_wrap (const mpfr_ptr p1 , const mpfr_ptr p2) {
  return mpfr_cmp(p1, p2)
}

int mpfr_cmp_si_wrap (const mpfr_ptr p1, signed long int p2) {
  return mpfr_cmp_si(p1, p2);
}

int mpfr_cmp_ui_wrap (const mpfr_ptr p1, unsigned long int p2) {
  return mpfr_cmp_ui (p1, p2)
}

int mpfr_sgn_wrap (const mpfr_ptr p1) {
  return mpfr_sgn (p1);
} 

int mpfr_set_si_wrap (const mpfr_ptr p, long int si, mp_rnd_t r) {
  return mpfr_set_si(p, si, r);
}

int mpfr_set_ui_wrap (const mpfr_ptr p, unsigned long int si, mp_rnd_t r) {
  return mpfr_set_ui(p, si, r);
}

int mpfr_ceil_wrap (const mpfr_ptr p, const mpfr_ptr p2) {
  return mpfr_ceil(p, p2);
}

int mpfr_floor_wrap (const mpfr_ptr p, const mpfr_ptr p2) {
  return mpfr_floor(p, p2);
}

int mpfr_round_wrap (const mpfr_ptr p, const mpfr_ptr p2) {
  return mpfr_round(p, p2);
}

int mpfr_trunc_wrap (const mpfr_ptr p, const mpfr_ptr p2) {
  return mpfr_trunc(p, p2);
}

mp_prec_t mpfr_get_prec_wrap (const mpfr_ptr e) {
  return mpfr_get_prec(e);
}

void * mpfr_custom_get_mantissa_wrap (const mpfr_ptr p) {
  return mpfr_custom_get_mantissa(p);
}

mp_exp_t mpfr_custom_get_exp_wrap(const mpfr_ptr p) {
  return mpfr_custom_get_exp(p);
}
