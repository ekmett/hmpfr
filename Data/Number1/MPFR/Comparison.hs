{-# INCLUDE <mpfr.h> #-}
{-# INCLUDE <chsmpfr.h> #-}

module Data.Number1.MPFR.Comparison where

import Data.Number1.MPFR.Internal

import Prelude hiding (isNaN)

cmp         :: Dyadic -> Dyadic -> Maybe Ordering
cmp mp1 mp2 = if isNaN mp1 || isNaN mp2 then Nothing 
                else Just (compare (withDyadicBB mp1 mp2 mpfr_cmp) 0)

cmpw       :: Dyadic -> Word -> Maybe Ordering
cmpw mp1 w = if isNaN mp1 then Nothing else Just (compare (unsafePerformIO go) 0)
    where go = with mp1 $ \p -> mpfr_cmp_ui p (fromIntegral w) 

cmpi       :: Dyadic -> Int -> Maybe Ordering
cmpi mp1 i = if isNaN mp1 then Nothing else Just (compare (unsafePerformIO go) 0)
    where go = with mp1 $ \p -> mpfr_cmp_si p (fromIntegral i)

cmpd       :: Dyadic -> Double -> Maybe Ordering
cmpd mp1 d = unsafePerformIO go
    where go = do mpfr_clear_erangeflag
                  with mp1 $ \p -> do
                    r1 <- mpfr_cmp_d p (realToFrac d)
                    r2 <- mpfr_erangeflag_p
                    if r2 == 0 then return (Just (compare r1 0))
                      else do mpfr_clear_erangeflag
                              return Nothing
                    
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
cmpabs mp1 mp2 = if isNaN mp1 || isNaN mp2 then Nothing 
                   else Just (compare (withDyadicBB mp1 mp2 mpfr_cmpabs) 0)

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
lessgreater d1 d2 = if isNaN d1 || isNaN d2 then Nothing 
                      else Just (withDyadicBB d1 d2 mpfr_lessgreater_p /= 0)

equal       :: Dyadic -> Dyadic -> Bool
equal d1 d2 = withDyadicBB d1 d2 mpfr_equal_p /= 0

unordered       :: Dyadic -> Dyadic -> Maybe Bool
unordered d1 d2 = if isNaN d1 || isNaN d2 then Nothing 
                    else Just (withDyadicBB d1 d2 mpfr_unordered_p /= 0)
