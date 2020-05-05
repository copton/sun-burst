{-# LANGUAGE EmptyDataDecls #-}
module SvgElements
    ( Point(..)
    , polarToCartesian
    ) where

import qualified Layout as L

import qualified Data.Text as T
import Data.Tagged (Tagged(Tagged, unTagged))
import Data.Ratio ((%))

data OffsetTag
type Offset = Tagged OffsetTag Rational

data LabelTag
type Label = Tagged LabelTag T.Text

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

data Circle = Circle
    { circleCenter :: Point
    , circleRadius :: Radius
    }

data Text = Text
    { textText  :: T.Text
    , textPoint :: Point
    , textPath  :: Maybe (Label, Path)
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
        radian = (fromIntegral angle) * pi / 180.0
        x = Tagged $ (fromIntegral radius) * (floatingToRational (cos radian))
        y = Tagged $ (fromIntegral radius) * (floatingToRational (sin radian))

sunBurstElements :: L.SunBurst -> [Element]
sunBurstElements (L.SunBurst center sectors) =
    centerElements center ++ concatMap sectorElements sectors

largeArcFlag :: L.Sector -> Bool
largeArcFlag (L.Sector _ start end) =
    L.polarAngle end - L.polarAngle start > 180

sectorElements :: L.Sector -> [Element]
sectorElements sector@(L.Sector label start end) = [EPath path]
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

centerElements :: L.Center -> [Element]
centerElements (L.Center label radius) =
    [ ECircle $ Circle (Point (Tagged 0) (Tagged 0)) (Tagged (unTagged radius))
    , EText $ Text label (Point (Tagged 0) (Tagged 0)) Nothing
    ]