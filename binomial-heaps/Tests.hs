module Tests where

import IC.TestSuite
import BinomialHeaps

-- test format:
-- testCases = [ expected-input-1 ==> expected-output 1 , ... ]
valueTestCases
 = [
     t4 ==> 2
 ]

childrenTestCases 
 = [
    t2 ==> [Node 5 0 []]
 ]

-- allTestCases format:
-- TestCase "testCase" (uncurry testCase) testCaseTestCases
allTestCases 
 = []

runTests = mapM_ goTest allTestCases

main = runTests