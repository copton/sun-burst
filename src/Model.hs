{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE EmptyDataDecls #-}

module Model (
    RootNode(..),
    Node(..),
    RelSize,
    assertRootNodeConsistency,
    assertNodeConsistency
) where

import qualified Data.Text as T
import Control.Monad.Except(throwError)
import Control.Monad (forM_, when)
import Data.Tagged(Tagged)

newtype RootNode = RootNode { getRootNode :: Node }

data RelSizeTag
type RelSize = Tagged RelSizeTag Int

data Node = Node
    { nodeLabel  :: T.Text
    , nodeSize   :: RelSize
    , nodeLeaves :: [Node]
    }

assertRootNodeConsistency :: RootNode -> Either T.Text ()
assertRootNodeConsistency (RootNode node@(Node label size _)) = do
    assertNodeConsistency node
    when (size /= 100) $
        throwError $ "Root node " <> label <> " has a size of " <> T.pack (show size)

assertNodeConsistency :: Node -> Either T.Text ()
assertNodeConsistency (Node _     _ []    ) = return ()
assertNodeConsistency (Node label _ leafes) = do
    forM_ leafes assertNodeConsistency
    let totalSize = sum $ map nodeSize leafes
    when (totalSize /= 100) $ 
        throwError $ "Leaves of node " <> label <> " sum up to " <> T.pack (show totalSize)
    return ()
