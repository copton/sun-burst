{-# LANGUAGE EmptyDataDecls #-}
module SvgElements
    ( sunBurstElements
    , Point(..)
    , Offset
    , Element(..)
    , Circle(..)
    , Text(..)
    , Path(..)
    , PathData(..)
    , Move(..)
    , Line(..)
    , Arc(..)
    , Close(..)
    , polarToCartesian
    ) where

import qualified Layout as L

import qualified Data.Text as T
import Data.Tagged (Tagged(Tagged, unTagged))
import Data.Ratio ((%))

data OffsetTag
type Offset = Tagged OffsetTag Rational

data RadiusTag
type Radius = Tagged RadiusTag Int

data Point = Point -- Cartesian Coordinates
    { pointX :: Offset
    , pointY :: Offset
    } deriving (Eq, Show)

data Element
    = ECircle Circle
    | EText Text
    | EPath Path

-- center at origin
data Circle = Circle
    { circleRadius :: Radius
    }

data Text = Text
    { textText  :: T.Text
    , textPath  :: Path
    }

data Path = Path
    { pathData :: [PathData]
    }

data PathData
    = PDMove Move
    | PDLine Line
    | PDArc Arc
    | PDClose Close

data Move = Move
    { movePoint :: Point
    }

data Line = Line
    { linePointTo :: Point
    }

data Arc = Arc
    { arcRadius :: Radius
    , arcEnd    :: Point
    , arcLarge  :: Bool
    , arcSweep  :: Bool
    }

data Close = Close

floatingToRational :: (RealFrac a, Floating a) => a -> Rational
floatingToRational x = round (x * (fromIntegral precision)) % precision
    where
        precision = 100000000

polarToCartesian :: L.PolarCoordinates -> Point
polarToCartesian (L.PolarCoordinates (Tagged angle) (Tagged radius)) =
        Point x y
    where
        radian = (fromIntegral angle) * pi / (180.0 :: Double)
        x = Tagged $ (fromIntegral radius) * (floatingToRational (cos radian))
        y = Tagged $ (fromIntegral radius) * (floatingToRational (sin radian))

sunBurstElements :: L.SunBurst -> [Element]
sunBurstElements (L.SunBurst center sectors) =
    centerElements center ++ concatMap sectorElements sectors

largeArcFlag :: L.Sector -> Bool
largeArcFlag (L.Sector _ start end) =
    L.polarAngle end - L.polarAngle start > 180

sectorElements :: L.Sector -> [Element]
sectorElements sector =
    [ sectorArcs sector
    , sectorText sector
    ]

sectorArcs :: L.Sector -> Element
sectorArcs sector@(L.Sector _ start end) = EPath path
    where
        path     = Path [move, innerArc, line, outerArc, close]
        move     = PDMove $ Move (polarToCartesian start)
        innerArc = PDArc $ Arc (Tagged $ unTagged $ L.polarRadius start)
                               (polarToCartesian (L.PolarCoordinates
                                    (L.polarAngle end) (L.polarRadius start)))
                                (largeArcFlag sector)
                                True
        line     = PDLine $ Line $ polarToCartesian end
        outerArc = PDArc $ Arc (Tagged $ unTagged $ L.polarRadius end)
                               (polarToCartesian (L.PolarCoordinates
                                    (L.polarAngle start) (L.polarRadius end)))
                                (largeArcFlag sector)
                                False
        close = PDClose Close

sectorText :: L.Sector -> Element
sectorText (L.Sector label start end) = EText $ Text label path
    where
        (L.PolarCoordinates alpha r) = start
        (L.PolarCoordinates beta  s) = end

        {-
        t = (r + s) `div` 2
        end = L.PolarCoordinates beta t

        path = Path [move]
        move = PDMove $ Move $ polarToCartesian $ L.PolarCoordinates alpha r
        arc = PDArc $ Arc (Tagged $ unTagged t) (polarToCartesian end) (largeArcFlag sector) True
        -}
        path = Path [move, line]
        move = PDMove $ Move $ polarToCartesian $ L.PolarCoordinates gamma r
        line = PDLine $ Line $ polarToCartesian $ L.PolarCoordinates gamma s
        gamma = (alpha + beta) `div` 2

centerElements :: L.Center -> [Element]
centerElements (L.Center label radius) =
        [ ECircle $ Circle (Tagged (unTagged radius))
        , EText text 
        ]
    where
        text = Text label path
        path = Path [move, line]
        move = PDMove $ Move $ polarToCartesian $ L.PolarCoordinates (Tagged 180) radius
        line = PDLine $ Line $ polarToCartesian $ L.PolarCoordinates (Tagged   0) radius