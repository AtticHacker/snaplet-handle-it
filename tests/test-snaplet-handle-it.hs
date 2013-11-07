module Main where

import Test.HUnit
import Test.Snap.Snaplet.HandleIt.Router as Router

unitTests :: [Test]
unitTests = concat
            [ Router.tests
            ]

main :: IO Counts
main = runTestTT $ test unitTests
