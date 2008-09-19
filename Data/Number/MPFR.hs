{-|
    Module      :  Data.Number.MPFR
    Description :  top level
    Copyright   :  (c) Aleš Bizjak
    License     :  BSD3

    Maintainer  :  ales.bizjak0@gmail.com
    Stability   :  experimental
    Portability :  portable

 /Naming/

  - functions ending with _ (underscore) usually return a pair (MPFR, Int), where
 Int is a return value of a corresponding mpfr_ function. See the MPFR manual for 
 a description of return values.

 - the same functions without the _ return just the MPFR. 

 - mpfr_ prefix in functions is removed

 - _ui and ui_ in function becomes w (stands for Word). For example mpfr_add_ui becomes addw.

 - si_ and _si in functions becomes i (stands for Int).

 - comparison functions which have _p appended loose it. For example mpfr_less_p becomes less.

   /Instances/

  Eq
 
   - NaN \/= Nan, 

   - Infinity = Infinity, 

   - \-Infinity = -Infinity

   - otherwise normal comparison 



  Ord      
 
   - compare NaN _ = GT

   - compare _ NaN = GT

   - infinity < _ = False

   - \-infinity > _ = False

   - NaN [\<,\>,\>=,<=] _ = False

   This mimics the behaviour of built in Haskell Float and Double.



 Num

 Operations defined in Num class will be computed exactly (no precision is lost).
 This isn't particularly useful as precision grows fairly quickly and everything becomes
 slow so it preferably shouldn't be used.



  /This module should always be imported qualified./

-}

{-# INCLUDE <mpfr.h> #-}
{-# INCLUDE <chsmpfr.h> #-}


module Data.Number.MPFR (
     module Data.Number.MPFR.Base
) where

import Data.Number.MPFR.Base

import Data.Number.MPFR.Internal

import Data.Maybe

import Data.Ratio

instance Num MPFR where
    d + d'        = add Zero (addPrec d d') d d'
    d - d'        = sub Zero (addPrec d d') d d'
    d * d'        = mul Zero (getPrec d + getPrec d') d d'
    negate d      = neg Zero (getPrec d) d
    abs d         = absD Zero (getPrec d) d
    signum d      = fromInt Zero minPrec (fromMaybe (-1) (sgn d))
    fromInteger i = fromIntegerA Zero (checkPrec $ binprec i) i
                    -- TODO works only partially

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


instance Real MPFR where
    toRational d = n % 2 ^ e
        where (n', e') = decompose d
              (n, e) = if e' >= 0 then ((n' * 2 ^ e'), 0)
                         else (n', - e')

