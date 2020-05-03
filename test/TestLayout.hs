{-# LANGUAGE OverloadedStrings #-}
module TestLayout (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Tagged(Tagged(Tagged))
import qualified Data.Text as T
import Data.List (sort)

import Layout
import Model

tests :: TestTree
tests = testGroup "layout test"
    [ testGroup "converting node relative size to angle" $ map testNodeSizeToAngle nodeSizeToAngleTests
    , testGroup "layouting a node without leaves" $ map testLayoutNodeNoLeaves layoutNodeNoLeavesTests 
    , testGroup "layouting a list of leaves" $ map testLayoutLeaves layoutLeavesTests
    , testGroup "layouting a root node" $ map testLayout layoutTests
    ]

nodeSizeToAngleTests :: [(Angle, Angle, RelSize, Angle)]
nodeSizeToAngleTests = map (\(a1, a2, s, a3) -> (Tagged a1, Tagged a2, Tagged s, Tagged a3))
    [ (  0, 360,   0,   0)
    , (  0, 360,  25,  90)
    , (  0, 360, 100, 360)
    , (180, 360,  50,  90)
    ]

testNodeSizeToAngle :: (Angle, Angle, RelSize, Angle) -> TestTree
testNodeSizeToAngle (start, end, size, angle) = testCase testLabel $
        angle @=? nodeSizeToAngle start end size
    where
        testLabel = "[" <> show start <> ", " <> show end <> "] " <> show size <> " -> " <> show angle

zoom :: Radius
zoom = Tagged 10

layoutNodeNoLeavesTests :: [(String, Layer, Angle, Node, Sector)]
layoutNodeNoLeavesTests = map (\(t, l, a, n, s) -> (t, Tagged l, Tagged a, n, s))
    [("100% size, no offset, layer 1"
     , 1
     , 0
     , Node "n" 100 undefined 
     , Sector "n"
        (PolarCoordinates (Tagged   0) (1 * zoom))
        (PolarCoordinates (Tagged 360) (2 * zoom))
     )
    ,("50% size, 45 degree offset, layer 1"
     , 1
     , 45
     , Node "n" 50 undefined 
     , Sector "n"
        (PolarCoordinates (Tagged  45) (1 * zoom))
        (PolarCoordinates (Tagged 225) (2 * zoom))
    )
    ,("10% size, 190 degree offset, layer 2"
     , 2
     , 190
     , Node "n" 10 undefined 
     , Sector "n"
        (PolarCoordinates (Tagged 190) (2 * zoom))
        (PolarCoordinates (Tagged 226) (3 * zoom))
    )
    ] 

testLayoutNodeNoLeaves :: (String, Layer, Angle, Node, Sector) -> TestTree
testLayoutNodeNoLeaves (testLabel, layer, angle, node, sector) = testCase testLabel $
        sector @=? fst (layoutNodeNoLeaves config layer (Tagged 0) (Tagged 360) angle node)
    where
        config = Config zoom

layoutLeavesTests :: [(String, [Node], [Sector])]
layoutLeavesTests =
    [("single node"
     ,[Node "n" 100 []
      ]
     ,[Sector "n"
        (PolarCoordinates (Tagged   0) (1 * zoom))
        (PolarCoordinates (Tagged 360) (2 * zoom))
      ]
     )
    ,("two nodes, 25/75"
     ,[Node "n1" 25 []
      ,Node "n2" 75 []
      ]
     ,[Sector "n1"
        (PolarCoordinates (Tagged   0) (1 * zoom))
        (PolarCoordinates (Tagged  90) (2 * zoom))
      ,Sector "n2"
        (PolarCoordinates (Tagged  90) (1 * zoom))
        (PolarCoordinates (Tagged 360) (2 * zoom))
      ]
     )
    ,("second ring incomplete"
     ,[Node "n1" 50 []
     ,Node "n2" 50
        [Node "n2.1" 50 []
        ,Node "n2.2" 50 []
        ]
      ]
     ,[Sector "n1"
        (PolarCoordinates (Tagged   0) (1 * zoom))
        (PolarCoordinates (Tagged 180) (2 * zoom))
      ,Sector "n2"
        (PolarCoordinates (Tagged 180) (1 * zoom))
        (PolarCoordinates (Tagged 360) (2 * zoom))
      ,Sector "n2.1"
        (PolarCoordinates (Tagged 180) (2 * zoom))
        (PolarCoordinates (Tagged 270) (3 * zoom))
      ,Sector "n2.2"
        (PolarCoordinates (Tagged 270) (2 * zoom))
        (PolarCoordinates (Tagged 360) (3 * zoom))
      ]
     )
    ]

testLayoutLeaves :: (String, [Node], [Sector]) -> TestTree
testLayoutLeaves (testLabel, nodes, sectors) = testCase testLabel $
        sort sectors @=? sort (layoutLeaves config (Tagged 1) (Tagged 0) (Tagged 360) nodes)
    where
        config = Config zoom

layoutTests :: [(RootNode, SunBurst)]
layoutTests = map (\(label, leaves, sb) -> (RootNode (Node label 100 leaves), sb))
    [( "no leaves"
     , []
     , SunBurst (Center "no leaves" zoom) []
     )
    ]

testLayout :: (RootNode, SunBurst) -> TestTree
testLayout (rootNode, sunBurst) = testCase testLabel $ 
        sunBurst @=? layout config rootNode
    where
        testLabel = (T.unpack . nodeLabel . getRootNode) rootNode 
        config = Config zoom