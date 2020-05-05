{-# LANGUAGE EmptyDataDecls #-}

module Layout
    ( layout
    , layoutNodeNoLeaves
    , layoutLeaves
    , Config(..)
    , Angle
    , Radius
    , Layer
    , Sector(..)
    , PolarCoordinates(..)
    , SunBurst(..)
    , Center(..)
    , nodeSizeToAngle
    ) where

import Model
import qualified Data.Text as T
import Data.Tagged (Tagged(Tagged, unTagged))

data AngleTag
type Angle = Tagged AngleTag Int

data RadiusTag
type Radius = Tagged RadiusTag Int

data LayerTag
type Layer = Tagged LayerTag Int

data PolarCoordinates = PolarCoordinates
    { polarAngle  :: Angle
    , polarRadius :: Radius
    } deriving (Show, Eq, Ord)

data Center = Center
    { centerLabel  :: T.Text
    , centerRadius :: Radius
    } deriving (Show, Eq)

data Sector = Sector
    { sectorLabel :: T.Text
    , sectorStart :: PolarCoordinates
    , sectorEnd   :: PolarCoordinates
    } deriving (Show, Eq, Ord)

data SunBurst = SunBurst
    { sunBurstCenter :: Center
    , sunBorstSectors :: [Sector]
    } deriving (Show, Eq)

data Config = Config
    { configZoom :: Radius
    }

layout :: Config -> RootNode -> SunBurst
layout config (RootNode (Node label _ leaves)) =
        SunBurst center sectors
    where
        center = Center label (configZoom config)
        sectors = layoutLeaves config (Tagged 1) (Tagged 0) (Tagged 360) leaves

layoutNodeNoLeaves :: Config -> Layer -> Angle -> Angle -> Angle -> Node -> (Sector, Angle)
layoutNodeNoLeaves config layer open close alpha (Node label size _) = (sector, beta)
    where
        zoom   = configZoom config
        beta   = alpha + nodeSizeToAngle open close size
        start  = PolarCoordinates alpha (applyZoomToLayer zoom  layer     )
        end    = PolarCoordinates beta  (applyZoomToLayer zoom (layer + 1))
        sector = Sector label start end

layoutLeaves :: Config -> Layer -> Angle -> Angle -> [Node] -> [Sector]
layoutLeaves config layer open close = snd . foldr go (open, []) . reverse
    where
        go node (alpha, ss) =
            let (sector, beta) = layoutNodeNoLeaves config layer open close alpha node
                leaveSectors = layoutLeaves config (layer + 1) alpha beta (nodeLeaves node)
            in
                (beta, sector : ss ++ leaveSectors)

nodeSizeToAngle :: Angle -> Angle -> RelSize -> Angle
nodeSizeToAngle (Tagged open) (Tagged close) (Tagged size) = Tagged $
    (close - open) * size `div` 100

applyZoomToLayer :: Radius -> Layer -> Radius
applyZoomToLayer zoom layer = zoom * Tagged (unTagged layer)