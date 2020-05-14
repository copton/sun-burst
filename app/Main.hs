{-# LANGUAGE OverloadedStrings #-}
module Main where

import Model
import Layout
import SvgElements
import SvgXml

import System.Process (proc, withCreateProcess, StdStream(CreatePipe), CreateProcess(std_in, std_out))
import System.IO (hGetContents, hClose)
import Data.Tagged (Tagged(Tagged))
import qualified Data.Text as T
import System.IO(withFile, IOMode(WriteMode), hPutStr)

model :: RootNode
model =
    RootNode $ Node "center" (Tagged 100)
        [ Node "1" (Tagged 10) []
        , Node "2" (Tagged 30) []
        , Node "3" (Tagged 60)
            [ Node "a" (Tagged 10) []
            , Node "b" (Tagged 60) []
            , Node "c" (Tagged 30) []
            ]
        ]

config :: Config
config = Config $ Tagged 20

sunBurst :: SunBurst
sunBurst = layout config model

elements :: [Element]
elements = sunBurstElements sunBurst

xmlString :: String
xmlString = toXml elements

assert :: Either T.Text a -> IO a
assert (Left x) = error (T.unpack x)
assert (Right x) = return x

main :: IO ()
main = do
    assert $ assertRootNodeConsistency model
    let p = (proc "xmlstarlet" ["fo", "-"]){std_in = CreatePipe, std_out = CreatePipe}
    withCreateProcess p $ \ (Just hIn) (Just hOut) _ _ -> do
        hPutStr hIn xmlString
        hClose hIn
        xmlString' <- hGetContents hOut
        withFile "/tmp/e.svg" WriteMode $ \h -> do
            hPutStr h xmlString' 