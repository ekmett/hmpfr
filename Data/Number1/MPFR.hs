{-# INCLUDE <mpfr.h> #-}
{-# INCLUDE <chsmpfr.h> #-}
module Data.Number1.MPFR (
     module Data.Number1.MPFR.Near
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

import Data.Number1.MPFR.Near









