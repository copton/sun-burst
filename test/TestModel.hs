{-# LANGUAGE OverloadedStrings #-}

module TestModel (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text as T

import Model

tests :: TestTree
tests = testGroup "model tests" 
    [ testGroup "valid nodes" 
        [ testGroup "root nodes" $ map (runTest testValidRootNode) validRootNodes
        , testGroup "non-root nodes" $ map (runTest testValidNode) validNodes
        ]
    , testGroup "invalid nodes" 
        [ testGroup "root nodes" $ map (runTest testInvalidRootNode) invalidRootNodes 
        , testGroup "non-root nodes" $ map (runTest testInvalidNode) invalidNodes 
        ]
    ]
        
runTest :: (Node -> Assertion) -> Node -> TestTree
runTest f n = testCase (T.unpack (nodeLabel n)) (f n)

validRootNodes, invalidRootNodes, validNodes, invalidNodes :: [Node]
validRootNodes =
    [ Node "without leaves" 100 []
    , Node "with leaves" 100 [Node "leave node 1" 50 [], Node "leave node 2" 50 []]
    ]

invalidRootNodes =
    [ Node "without leaves" 99 []
    , Node "with leaves" 100 [Node "leave no 1" 50 [], Node "leave node 2" 10 []]
    ]

validNodes =
    [ Node "without leaves" 99 []
    , Node "with leaves" 100 [Node "leave node 1" 50 [], Node "leave node 2" 50 []]
    ]

invalidNodes =
    [ Node "with leaves" 100 [Node "leave no 1" 50 [], Node "leave node 2" 10 []]
    ]

testValid, testInvalid :: (Node -> Either T.Text ()) -> Node -> Assertion
testValid assert node = do
    case assert node of
        Left e -> assertFailure (T.unpack e)
        Right () -> return ()

testInvalid assert node = do
    case assert node of
        Left _ -> return ()
        Right () -> assertFailure $ (T.unpack (nodeLabel node)) <> " is unexpectedly valid"

testValidRootNode, testInvalidRootNode, testValidNode, testInvalidNode :: Node -> Assertion
testValidRootNode = testValid (\n -> assertRootNodeConsistency (RootNode n))
testInvalidRootNode = testInvalid (\n -> assertRootNodeConsistency (RootNode n))
testValidNode = testValid assertNodeConsistency
testInvalidNode = testInvalid assertNodeConsistency