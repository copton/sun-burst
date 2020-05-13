{-# LANGUAGE LambdaCase #-}
module SvgXml
    ( toXml
    ) where

import qualified SvgElements as E

import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Data.Tagged (Tagged(Tagged, unTagged))
import Text.XML.HaXml.Types
import Text.XML.HaXml.Pretty (document)
import Text.XML.HaXml (render)
import Text.Printf (printf)
import qualified Data.Text as T

toXml :: [E.Element] -> String
toXml = render . document . xml

index :: [a] -> [(Int, a)]
index xs = zip [1..] xs

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
body elements = group
    where   
        group = CElem (Elem (N "g") [] (defs : drawings)) ()
        indexedElements = zip [1..] elements
        defs = CElem (Elem (N "defs") [] (mapMaybe elemTextPath indexedElements)) ()
        drawings = map element indexedElements
        
element :: (Int, E.Element) -> Content ()
element (idx, e )= case e of
    E.ECircle x -> CElem (elemCircle x) ()
    E.EText x -> CElem (elemText idx x) ()
    E.EPath x -> CElem (elemPath x) ()

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

elemText :: Int -> E.Text -> Element ()
elemText idx text =
        Elem (N "g") [] [CElem eu (), CElem et ()]
    where
        et = Elem (N "text") [a] [CElem path ()]
        a = attr "text-anchor" "middle"
        path = Elem (N "textPath") [o, l] [txt']
        (E.Text txt _) = text
        txt' = CString False (T.unpack txt) ()
        o = attr "startOffset" "50%"
        l = attr "xlink:href" (href False idx)

        eu = Elem (N "use") [l, s, f] [] -- for debugging only
        f = attr "fill" "none"
        s = attr "stroke" "red"
        
href :: Bool -> Int -> String
href isDef idx = hash <> "path" <> show idx
    where   
        hash = if isDef then "" else "#"

elemTextPath :: (Int, E.Element) -> Maybe (Content ())
elemTextPath (idx, E.EText (E.Text _ (E.Path pd))) = Just $ CElem textPath ()
    where
        textPath = Elem (N "path") [i, d] []
        i = attr "id" (href True idx)
        d = attr "d" $ intercalate " " $ map pathData pd
elemTextPath _ = Nothing

elemPath :: E.Path -> Element ()
elemPath (E.Path pd) =
        Elem (N "path") [s, sw, f, d] []
    where
        s = attr "stroke" "black"
        sw = attr "stroke-width" "1"
        f = attr "fill" "none"
        d = attr "d" $ intercalate " " $ map pathData pd

pathData :: E.PathData -> String
pathData = \case
        E.PDMove (E.Move (E.Point (Tagged x) (Tagged y))) ->
            intercalate " "
                ["M", show (fromRational x), show (fromRational y)]

        E.PDLine (E.Line (E.Point (Tagged x) (Tagged y))) ->
            intercalate " "
                ["L", show (fromRational x), show (fromRational y)]

        E.PDArc (E.Arc (Tagged r) (E.Point (Tagged x) (Tagged y)) l s) ->
            intercalate " "
                [ "A", show r, show r, "0", flag l, flag s
                , show (fromRational x), show (fromRational y)]

        E.PDClose (E.Close) -> "Z"
    where
        flag True = "1"
        flag False = "0"

attr :: String -> String -> Attribute
attr n v = (N n, AttValue [Left v])

viewBox :: [E.Element] -> Int
viewBox = maximum . concatMap viewBoxElement
    where
        viewBoxElement = \case
            E.ECircle (E.Circle (Tagged radius)) -> [radius]
            E.EText (E.Text _ (E.Path d)) -> concatMap viewBoxPathData d
            E.EPath (E.Path d) -> concatMap viewBoxPathData d
        
        viewBoxPathData = \case
            E.PDMove (E.Move x)     -> viewBoxPoint x
            E.PDLine (E.Line x)     -> viewBoxPoint x
            E.PDArc (E.Arc _ x _ _) -> viewBoxPoint x
            E.PDClose _             -> []

        viewBoxPoint (E.Point (Tagged x) (Tagged y)) = [ceiling' x, ceiling' y]

        ceiling' = ceiling . fromRational