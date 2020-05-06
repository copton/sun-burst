{-# LANGUAGE LambdaCase #-}
module SvgXml
    (
    ) where

import qualified SvgElements as E

import Data.Tagged (Tagged(Tagged, unTagged))
import Text.XML.HaXml.Types
import Text.XML.HaXml.Pretty (document)
import Text.XML.HaXml (render)

toXml :: [E.Element] -> String
toXml = render . document . xml

xml :: [E.Element] -> Document ()
xml elements = Document prolog [] (root elements) []

prolog :: Prolog
prolog = Prolog (Just decl) [] (Just dtd) []
    where
        decl = XMLDecl "1.0" (Just (EncodingDecl "utf-8")) (Just False)
        dtd = DTD (N "svg") (Just eid) []
        eid = PUBLIC (PubidLiteral pl) (SystemLiteral sl)
        pl = "-//W3C//DTD SVG 1.1//EN"
        sl = "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd"

root :: [E.Element] -> Element ()
root elements = Elem (N "svg") attrs (content elements)
    where
        attrs = [width, height, vb, ns, version]
        ns = (N "xmlns:xlink", AttValue [Left "http://www.w3.org/1999/xlink"])
        vb = undefined
        width = (N "width", AttValue [Left "100%"])
        height = (N "height", AttValue [Left "100%"])
        version = (N "version", AttValue [Left "1.1"])

content :: [E.Element] -> [Content ()]
content = undefined

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