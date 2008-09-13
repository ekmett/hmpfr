{-# LANGUAGE TypeSynonymInstances #-}
{-# INCLUDE <mpfr.h> #-}
{-# INCLUDE <chsmpfr.h> #-}
module Data.Number1.MPFR (
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

) where


--conversion from dyadics to basic haskell types
 
------------------------------------
-- mpfr constants

----------------------------------------------------------
-- conversion from basic haskell types to dyadics


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


