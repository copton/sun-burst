{-# LANGUAGE LambdaCase #-}
module SvgXml
    (
    ) where

import qualified SvgElements as E

import Data.Tagged (Tagged(Tagged, unTagged))

viewBox :: [E.Element] -> Int
viewBox = maximum . concatMap viewBoxElement
    where
        viewBoxElement = \case
            E.ECircle (E.Circle (Tagged radius)) -> [radius]
            E.EText _ -> undefined
            E.EPath (E.Path d) -> concatMap viewBoxPathData d
        
        viewBoxPathData = \case
            E.PDMove (E.Move x)     -> viewBoxPoint x
            E.PDLine (E.Line x)     -> viewBoxPoint x
            E.PDArc (E.Arc _ x _ _) -> viewBoxPoint x
            E.PDClose _             -> []

        viewBoxPoint (E.Point (Tagged x) (Tagged y)) = [ceiling' x, ceiling' y]

        ceiling' = ceiling . fromRational