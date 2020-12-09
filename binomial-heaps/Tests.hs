module Tests where

import IC.TestSuite
import BinomialHeaps

-- test format:
-- testCases = [ expected-input-1 ==> expected-output 1 , ... ]

-- allTestCases format:
-- TestCase "testCase" (uncurry testCase) testCaseTestCases
allTestCases = []

runTests = mapM_ goTest allTestCases

main = runTests