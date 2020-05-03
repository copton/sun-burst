import Test.Tasty

import qualified TestModel as TestModel
import qualified TestLayout as TestLayout

main :: IO ()
main = defaultMain $ testGroup "sun burst tests"
    [ TestModel.tests
    , TestLayout.tests
    ]
