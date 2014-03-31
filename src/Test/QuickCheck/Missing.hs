{-# LANGUAGE OverloadedStrings     #-}

{-# OPTIONS -fwarn-unused-imports #-}

module Test.QuickCheck.Missing
where

import Data.List
import Test.QuickCheck


-- | missing in quickcheck: arbitrary choice of @n@ elements of a
-- list, in arbitrary order.  if an element occurs several times in
-- the input, it may occur as many times in the output.  if the
-- requested output size is smaller than the input set size, the
-- entire set is returned.  negative output size will crash.
subsetof :: Eq a => Int -> [a] -> Gen [a]
subsetof _ [] = return []
subsetof 0 _ = return []
subsetof n xs@(_:_) | n > 0 = do
    y <- elements xs
    ys <- subsetof (n-1) (xs \\ [y])
    return $ y:ys

thorough :: Testable prop => prop -> IO ()
thorough = quickCheckWith (stdArgs { maxSuccess = 1000000 }) . mapSize (*7)

-- | (missing in quickcheck?)
mkprop :: Bool -> Property
mkprop b = True ==> b

-- | (missing in quickcheck?)
pending :: Property
pending = counterexample "(pending)" False

