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

toStringExp       :: Word -> Dyadic -> String
toStringExp dec d = s ++ case e > 0 of
                           True  -> case Prelude.floor (logBase 10 2 * fromIntegral (getExp d) :: Double) > dec  of
                                      False -> take e ss ++ let bt = backtrim (drop e ss) in if null bt then "" else "." ++ bt
                                      True  -> head ss : "." ++ let bt = (backtrim . tail) ss in if null bt then "0"
                                                                                                   else bt ++ "e" ++ show (pred e)
                           False -> head ss : "." ++ (let bt = (backtrim . tail) ss in
                                                     if null bt then "0" 
                                                       else bt )
                                                  ++ "e" ++ show (pred e)
                    where (str, e') = dyadicToString Near n 10 d
                          e = fromIntegral e'
                          n        = max dec 5
                          (s, ss) = case head str of
                                      '-' -> ("-", tail str)
                                      _   -> ("" , str)
                          backtrim = reverse . dropWhile (== '0') . reverse 

toString       :: Word -> Dyadic -> String
toString dec d = s ++ case compare 0 e of
                         LT -> take e ss ++ (let bt = all (== '0') (drop e ss) in if bt then "" else '.' : (drop e ss))
                               ++ (if fromIntegral n - e < 0 then "e" ++ show (e - fromIntegral n) else "")
                         GT -> let ee = fromIntegral dec + e in 
                               if ee <= 0 then "0" else 
                                   head ss : "." ++ (backtrim . tail . take ee) ss ++ "e" ++ show (pred e)
                         EQ -> "0." ++ let bt = all (== '0') ss in if bt then "0" else ss
                  where (str, e') = dyadicToString Near n 10 d
                        n        = max dec 5
                        e = fromIntegral e'
                        (s, ss) = case head str of
                                    '-' -> ("-", tail str)
                                    _   -> ("" , str)
                        backtrim = reverse . dropWhile (== '0') . reverse 

 
------------------------------------
-- mpfr constants

----------------------------------------------------------
-- conversion from basic haskell types to dyadics


fromIntegerA       :: RoundMode -> Precision -> Integer -> Dyadic
fromIntegerA r p d = stringToDyadic r p 10 (show d)

compose             :: RoundMode -> Precision -> (Integer, Int) -> Dyadic 
compose r p (i, ii) = div2i r p (fromIntegerA r p i) ii

fromString       :: String -> Precision -> Word -> Dyadic
fromString s p b = stringToDyadic Near p b s

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
minPrec :: Precision
minPrec = 32

one ::  Dyadic              
one = fromWord Near minPrec 1

zero :: Dyadic              
zero = fromWord Near minPrec 0

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


