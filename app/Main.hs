module Main where

import Text.XML.HaXml as X
import Text.XML.HaXml.Pretty as P

main :: IO ()
main = do
    xml <- readFile "example.svg"
    let d1 = X.xmlParse "example.svg" xml
    let d2 = P.document d1
    let d3 = X.render d2
--    doc <- X.readFile def "example.svg"
    putStr d3
