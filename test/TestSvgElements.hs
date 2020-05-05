{-# LANGUAGE OverloadedStrings #-}

module TestSvgElements (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text as T
import Data.Tagged (Tagged(Tagged, unTagged))

import qualified Layout as L
import qualified SvgElements as S

tests :: TestTree
tests = testGroup "SVG backend tests" 
    [ testGroup "converting polars to cartesian coordinates" $
        map testPolarToCartesian polarToCartesianTests
    ]

polarToCartesianTests :: [(L.PolarCoordinates, S.Point)]
polarToCartesianTests = map addTags $
        [ ( 0, 10, 10.0,  0.0)
        , (90, 10,  0.0, 10.0)
        ]
    where
        addTags (a, r, x, y) =
            ( L.PolarCoordinates (Tagged a) (Tagged r)
            , S.Point (Tagged x) (Tagged y)
            )

testPolarToCartesian :: (L.PolarCoordinates, S.Point) -> TestTree
testPolarToCartesian (polar, point) = testCase testLabel $
        point @=? S.polarToCartesian polar
    where
        L.PolarCoordinates a r = polar
        S.Point x y = point
        testLabel = "(" <> show a <> ", " <> show r
                 <> ") -> (" <> show x <> ", " <> show y <> ")"