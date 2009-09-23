{-# LANGUAGE Rank2Types #-}

module TestArithmetic where    

import Test.QuickCheck

import Data.Number.MPFR.Internal

import Data.Maybe
    
import Prelude hiding(isNaN, exponent, isInfinite)

import TestBase

import qualified Data.Number.MPFR as M
import Data.Number.MPFR(MPFR, RoundMode, Precision)


import Control.Monad.ST

import qualified Data.Number.MPFR.Mutable as MM
import Data.Number.MPFR.Mutable(MMPFR)

prop_add = prop_arith1 MM.add M.add
prop_sub = prop_arith1 MM.sub M.sub
prop_mul = prop_arith1 MM.mul M.mul
prop_div = prop_arith1 MM.div M.div
prop_pow = prop_arith1 MM.pow M.pow

prop_addw = prop_arith2 MM.addw M.addw
prop_addi = prop_arith2 MM.addi M.addi
prop_addd = prop_arith2 MM.addd M.addd

prop_subw = prop_arith2 MM.subw M.subw
prop_subi = prop_arith2 MM.subi M.subi
prop_subd = prop_arith2 MM.subd M.subd

prop_wsub = prop_arith3 MM.wsub M.wsub
prop_isub = prop_arith3 MM.isub M.isub
prop_dsub = prop_arith3 MM.dsub M.dsub

prop_mulw = prop_arith2 MM.mulw M.mulw
prop_muli = prop_arith2 MM.muli M.muli
prop_muld = prop_arith2 MM.muld M.muld

prop_divw = prop_arith2 MM.divw M.divw
prop_divi = prop_arith2 MM.divi M.divi
prop_divd = prop_arith2 MM.divd M.divd

prop_wdiv = prop_arith3 MM.wdiv M.wdiv
prop_idiv = prop_arith3 MM.idiv M.idiv
prop_ddiv = prop_arith3 MM.ddiv M.ddiv


prop_arith1 :: (forall s. MMPFR s -> MMPFR s -> MMPFR s -> RoundMode -> ST s Int) 
            -> (RoundMode -> Precision -> MPFR -> MPFR -> MPFR)
            -> RoundMode -> MPFR -> MPFR -> Bool
prop_arith1 f g r m1 m2 = let r1 = runST go  
                              r2 = g r (M.getPrec m1) m1 m2
                          in M.isNaN r1 && M.isNaN r2 || r1 == r2
    where go = do m1' <- MM.thaw m1
                  m2' <- MM.unsafeThaw m2
                  f m1' m1' m2' r
                  MM.unsafeFreeze m1'
                                    
prop_arith2 :: Num a => (forall s. MMPFR s -> MMPFR s -> a -> RoundMode -> ST s Int) 
            -> (RoundMode -> Precision -> MPFR -> a -> MPFR)
            -> RoundMode -> MPFR -> a -> Bool
prop_arith2 f g r m1 m2 = let r1 = runST go  
                              r2 = g r (M.getPrec m1) m1 m2
                          in M.isNaN r1 && M.isNaN r2 || r1 == r2
    where go = do m1' <- MM.thaw m1
                  f m1' m1' m2 r
                  MM.unsafeFreeze m1'

prop_arith3 :: Num a => (forall s. MMPFR s -> a -> MMPFR s -> RoundMode -> ST s Int) 
            -> (RoundMode -> Precision -> a -> MPFR -> MPFR)
            -> RoundMode -> a -> MPFR -> Bool
prop_arith3 f g r m1 m2 = let r1 = runST go  
                              r2 = g r (M.getPrec m2) m1 m2
                          in M.isNaN r1 && M.isNaN r2 || r1 == r2
    where go = do m2' <- MM.thaw m2
                  f m2' m1 m2' r
                  MM.unsafeFreeze m2'
                                    
                  