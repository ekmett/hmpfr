{-# INCLUDE <mpfr.h> #-}
{-# INCLUDE <chsmpfr.h #-}

module Data.Number1.MPFR.Conversion where

import Data.Number1.MPFR.Internal

toDouble       :: RoundMode -> Dyadic -> Double
toDouble r mp1 = (realToFrac . unsafePerformIO) go
    where go = with mp1 $ \p -> mpfr_get_d p ((fromIntegral . fromEnum) r)

toDouble2exp     :: RoundMode -> Dyadic -> (Double, Int)
toDouble2exp r mp1 = unsafePerformIO go 
    where go = do with mp1 $ \p1 -> do
                    alloca $ \p2 -> do
                      r1 <- mpfr_get_d_2exp p2 p1 ((fromIntegral . fromEnum) r)
                      r2 <- peek p2
                      return (realToFrac r1, fromIntegral r2)
                      
toInt     :: RoundMode -> Dyadic -> Int
toInt r mp1 = (fromIntegral . unsafePerformIO) go
    where go = with mp1 $ \p -> mpfr_get_si p ((fromIntegral . fromEnum) r)

toWord       :: RoundMode -> Dyadic -> Word
toWord r mp1 = (fromIntegral . unsafePerformIO) go
    where go = with mp1 $ \p -> mpfr_get_ui p ((fromIntegral . fromEnum) r)


dyadicToString           :: RoundMode 
                   -> Word -- ^ number of decimals
                   -> Word -- ^ base
                   -> Dyadic -> (String, Exp)
dyadicToString r n b mp1 = unsafePerformIO go 
    where go = with mp1 $ \p1 -> do
                 alloca $ \p2 -> do
                     p3 <- mpfr_get_str nullPtr p2 (fromIntegral b) (fromIntegral n) p1 ((fromIntegral . fromEnum) r)
                     r1 <- peekCString p3 
                     r2 <- peek p2
                     mpfr_free_str p3
                     return (r1, r2)

fitsULong     :: RoundMode -> Dyadic -> Bool
fitsULong r d = withDyadicF d r mpfr_fits_ulong_p /= 0 

fitsSLong     :: RoundMode -> Dyadic -> Bool
fitsSLong r d = withDyadicF d r mpfr_fits_slong_p /= 0 

fitsUInt     :: RoundMode -> Dyadic -> Bool
fitsUInt r d = withDyadicF d r mpfr_fits_uint_p /= 0 

fitsSInt     :: RoundMode -> Dyadic -> Bool
fitsSInt r d = withDyadicF d r mpfr_fits_sint_p /= 0 

fitsUShort     :: RoundMode -> Dyadic -> Bool
fitsUShort r d = withDyadicF d r mpfr_fits_ushort_p /= 0 

fitsSShort     :: RoundMode -> Dyadic -> Bool
fitsSShort r d = withDyadicF d r mpfr_fits_sshort_p /= 0 

-- TODO
decompose   :: Dyadic -> (Integer, Exp)
decompose d = (getMantissa d, getExp d - fromIntegral (Prelude.ceiling (fromIntegral (getPrec d) / fromIntegral bitsPerMPLimb :: Double) * bitsPerMPLimb))


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

