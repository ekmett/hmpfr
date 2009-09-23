module TestCompare where    

import Test.QuickCheck

import Data.Number.MPFR.Internal

import Data.Maybe
    
import Prelude hiding(isNaN, exponent, isInfinite)

import TestBase

import qualified Data.Number.MPFR.Comparison as C

prop_cmp_aux2 f g a b = f a b == g a b
prop_cmp_aux1 f g a = f a == g a

prop_cmp = prop_cmp_aux2 C.cmp cmp 
prop_isNaN = prop_cmp_aux1 C.isNaN isNaN
prop_isNumber = prop_cmp_aux1 C.isNumber isNumber
prop_isZero = prop_cmp_aux1 C.isZero isZero
prop_isInfinite = prop_cmp_aux1 C.isInfinite isInfinite
prop_sgn = prop_cmp_aux1 C.sgn sgn

cmp :: MPFR -> MPFR -> Maybe Ordering
cmp mp1 mp2 = unsafePerformIO go
    where go = do mpfr_clear_erangeflag
                  r1 <- with mp1 $ with mp2 . mpfr_cmp
                  r2 <- mpfr_erangeflag_p
                  if r2 == 0 then return (Just (compare r1 0))
                    else do mpfr_clear_erangeflag
                            return Nothing
                    
cmpw       :: MPFR -> Word -> Maybe Ordering
cmpw mp1 w = if isNaN mp1 then Nothing else Just (compare (unsafePerformIO go) 0)
    where go = with mp1 $ \p -> mpfr_cmp_ui p (fromIntegral w) 

cmp2w       :: MPFR -> Word -> Exp -> Maybe Ordering
cmp2w d w e = unsafePerformIO go
    where go = do mpfr_clear_erangeflag
                  with d $ \p -> do
                    r1 <- mpfr_cmp_ui_2exp p (fromIntegral w) e
                    r2 <- mpfr_erangeflag_p
                    if r2 == 0 then return (Just (compare r1 0))
                      else do mpfr_clear_erangeflag
                              return Nothing

cmp2i       :: MPFR -> Int -> Exp -> Maybe Ordering
cmp2i d w e = unsafePerformIO go
    where go = do mpfr_clear_erangeflag
                  with d $ \p -> do
                    r1 <- mpfr_cmp_si_2exp p (fromIntegral w) e
                    r2 <- mpfr_erangeflag_p
                    if r2 == 0 then return (Just (compare r1 0))
                      else do mpfr_clear_erangeflag
                              return Nothing

isNaN   :: MPFR -> Bool
isNaN d = withMPFRB d mpfr_nan_p /= 0

isInfinite   :: MPFR -> Bool
isInfinite d = withMPFRB d mpfr_inf_p /= 0 

isNumber   :: MPFR -> Bool
isNumber d = withMPFRB d mpfr_number_p /= 0 

isZero   :: MPFR -> Bool
isZero d = withMPFRB d mpfr_zero_p /= 0

sgn     :: MPFR -> Maybe Int 
sgn mp1 = case (cmpw mp1 0) of
            Nothing -> Nothing
            Just x -> Just (pred . fromEnum $ x)

greater       :: MPFR -> MPFR -> Bool
greater d1 d2 = withMPFRBB d1 d2 mpfr_greater_p /= 0

greatereq       :: MPFR -> MPFR -> Bool
greatereq d1 d2 = withMPFRBB d1 d2 mpfr_greaterequal_p /= 0

less       :: MPFR -> MPFR -> Bool
less d1 d2 = withMPFRBB d1 d2 mpfr_less_p /= 0

lesseq       :: MPFR -> MPFR -> Bool
lesseq d1 d2 = withMPFRBB d1 d2 mpfr_lessequal_p /= 0

lessgreater       :: MPFR -> MPFR -> Maybe Bool
lessgreater d1 d2 = if isNaN d1 || isNaN d2 then Nothing 
                      else Just (withMPFRBB d1 d2 mpfr_lessgreater_p /= 0)

equal       :: MPFR -> MPFR -> Bool
equal d1 d2 = withMPFRBB d1 d2 mpfr_equal_p /= 0

unordered       :: MPFR -> MPFR -> Maybe Bool
unordered d1 d2 = if isNaN d1 || isNaN d2 then Nothing 
                    else Just (withMPFRBB d1 d2 mpfr_unordered_p /= 0)

