-- File created: 2009-01-06 13:08:00

module Tests.Strictness (tests) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit.Base

tests =
   [ testGroup "Strictness" $
        flip map testCases $ \(name,ass) ->
           testCase name . assertBool "failed" $ ass
   ]

testCases =
   [ -- TODO
   ]
