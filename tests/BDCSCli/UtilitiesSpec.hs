module BDCSCli.UtilitiesSpec(main, spec)
  where

import Test.Hspec
import BDCSCli.Utilities

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Utility functions" $
        it "argify strings" $
            argify ["first,", "second", "third,fourth"] `shouldBe` ["first", "second", "third", "fourth"]
