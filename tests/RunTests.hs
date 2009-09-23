module Main where

import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2(testProperty)

import TestCompare

import TestArithmetic


main = do defaultMainWithOpts tests opts

opts = RunnerOptions (Just 3) (Just testopts) Nothing

testopts = TestOptions (Just RandomSeed) (Just 10000) Nothing Nothing

tests = [testGroup "Comparison functions" 
                       [testProperty "cmp" prop_cmp,
                        testProperty "isNaN" prop_isNaN,
                        testProperty "isNumber" prop_isNumber,
                        testProperty "isZero" prop_isZero,
                        testProperty "isInfinite" prop_isInfinite,
                        testProperty "sgn" prop_sgn],
         testGroup "Basic arithmetic functions"
                       [testProperty "add" prop_add,
                        testProperty "sub" prop_sub,
                        testProperty "mul" prop_mul,
                        testProperty "div" prop_div,
                                     
                        testProperty "addw" prop_addw,
                        testProperty "addi" prop_addi,
                        testProperty "addd" prop_addd,

                        testProperty "subw" prop_subw,
                        testProperty "subi" prop_subi,
                        testProperty "subd" prop_subd,
                        testProperty "wsub" prop_wsub,
                        testProperty "isub" prop_isub,
                        testProperty "dsub" prop_dsub,

                        testProperty "mulw" prop_mulw,
                        testProperty "muli" prop_muli,
                        testProperty "muld" prop_muld,

                        testProperty "divw" prop_divw,
                        testProperty "divi" prop_divi,
                        testProperty "divd" prop_divd,
                        testProperty "wdiv" prop_wdiv,
                        testProperty "idiv" prop_idiv,
                        testProperty "ddiv" prop_ddiv

                       ]
        ]

                        
                        

    
