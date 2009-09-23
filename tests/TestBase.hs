module TestBase where

import Test.QuickCheck

import Data.Word(Word)

import Data.Number.MPFR
import Data.Number.MPFR.Instances.Up
instance Arbitrary MPFR where
    arbitrary = do x <- arbitrarySizedIntegral
                   y <- arbitrarySizedIntegral
                   frequency [(49, return $ (fromInteger x / fromInteger y)), (1, elements [1/0, -1/0, 0/0])]

instance Arbitrary RoundMode where
    arbitrary = elements [Up, Down, Zero, Near]

instance Arbitrary Word where
    arbitrary = arbitraryBoundedIntegral