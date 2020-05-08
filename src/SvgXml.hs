{-# LANGUAGE LambdaCase #-}
module SvgXml
    ( toXml
    ) where

import qualified SvgElements as E

import Data.Tagged (Tagged(Tagged, unTagged))
import Text.XML.HaXml.Types
import Text.XML.HaXml.Pretty (document)
import Text.XML.HaXml (render)
import Text.Printf (printf)
import qualified Data.Text as T

toXml :: [E.Element] -> String
toXml = render . document . xml

xml :: [E.Element] -> Document ()
xml elements = Document prolog [] (root elements) []

prolog :: Prolog
prolog = Prolog (Just decl) [] (Just dtd) []
    where
        decl = XMLDecl "1.0" (Just (EncodingDecl "utf-8")) (Just True)
        dtd = DTD (N "svg") (Just eid) []
        eid = PUBLIC (PubidLiteral pl) (SystemLiteral sl)
        pl = "-//W3C//DTD SVG 1.1//EN"
        sl = "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd"

root :: [E.Element] -> Element ()
root elements = Elem (N "svg") attrs [body elements]
    where
        attrs = [width, height, vb, ns, version, style]
        ns = attr "xmlns:xlink" "http://www.w3.org/1999/xlink"
        style = attr "xmlns" "http://www.w3.org/2000/svg"
        vb = attr "viewBox" (printf "-%d -%d %d %d" halfSize halfSize size size)
        width = attr "width" "100%"
        height = attr "height" "100%"
        version = attr "version" "1.1"
        size = 2 * viewBox elements
        halfSize = size `div` 2

body :: [E.Element] -> Content ()
body elements = group (map element elements)
    where   
        group es = CElem (Elem (N "g") [trans] es) ()
        trans = attr "transform" "scale(1, -1)"

element :: E.Element -> Content ()
element = \case
    E.ECircle x -> CElem (elemCircle x) ()
    E.EText x -> CElem (elemText x) ()

elemCircle :: E.Circle -> Element ()
elemCircle (E.Circle (Tagged r)) =
        Elem (N "circle") [cx, cy, r', s, sw, f] []
    where
        cx = attr "cx" "0"
        cy = attr "cy" "0"
        r' = attr "r" (show r)
        sw = attr "stroke-width" "1"
        s  = attr "stroke" "black"
        f  = attr "fill" "none"

elemText :: E.Text -> Element ()
elemText (E.Text txt (E.Point (Tagged x) (Tagged y)) Nothing) =
        Elem (N "text") [x', y', t] [CString False (T.unpack txt) ()]
    where
        x' = attr "x" (show $ fromRational x)
        y' = attr "y" (show $ fromRational y)
        a = attr "text-anchor" "middle"
        t = attr "transform" "scale(1, -1)"

attr :: String -> String -> Attribute
attr n v = (N n, AttValue [Left v])

viewBox :: [E.Element] -> Int
viewBox = maximum . concatMap viewBoxElement
    where
        viewBoxElement = \case
            E.ECircle (E.Circle (Tagged radius)) -> [radius]
            E.EText _ -> []
            E.EPath (E.Path d) -> concatMap viewBoxPathData d
        
        viewBoxPathData = \case
            E.PDMove (E.Move x)     -> viewBoxPoint x
            E.PDLine (E.Line x)     -> viewBoxPoint x
            E.PDArc (E.Arc _ x _ _) -> viewBoxPoint x
            E.PDClose _             -> []

        viewBoxPoint (E.Point (Tagged x) (Tagged y)) = [ceiling' x, ceiling' y]

        ceiling' = ceiling . fromRational