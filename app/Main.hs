{-# LANGUAGE OverloadedStrings #-}
module Main where

import Model
import Layout
import SvgElements
import SvgXml

import Data.Tagged (Tagged(Tagged))
import qualified Data.Text as T

model :: RootNode
model = RootNode $ Node "center" (Tagged 100) []

config :: Config
config = Config $ Tagged 20

sunBurst :: SunBurst
sunBurst = layout config model

elements :: [Element]
elements = sunBurstElements sunBurst

xmlString :: String
xmlString = toXml elements

main :: IO ()
main = do
    case assertRootNodeConsistency model of
        Left x -> error (T.unpack x)
        Right () -> putStr xmlString