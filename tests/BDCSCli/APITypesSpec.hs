-- Copyright (C) 2017 Red Hat, Inc.
--
-- This file is part of bdcs-cli.
--
-- bdcs-cli is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- bdcs-cli is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with bdcs-cli.  If not, see <http://www.gnu.org/licenses/>.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module BDCSCli.APITypesSpec(main, spec)
  where

import           Data.Aeson(decode, fromJSON, toJSON, Result(..))
import qualified Data.ByteString.Lazy as BSL
import           Data.Maybe(fromJust)
import           Data.String.QQ
import           Test.Hspec

import           BDCSCli.Recipe(RecipeModule(..))
import           BDCSCli.API.Types.RecipeDiff


-- | A list of diff entries
-- This includes a changed description, a removed module, and added package, and a changed package
exampleDiff :: [RecipeDiffEntry]
exampleDiff =
    [ RecipeDiffEntry {
        rdeOld = Just Description {rdtDescription = "This is the old description"},
        rdeNew = Just Description {rdtDescription = "The New and Improved description"}
      }
    , RecipeDiffEntry {
        rdeOld = Just Module {rdtModule = RecipeModule {rmName = "tmux", rmVersion = "2.2"}},
        rdeNew = Nothing
      }
    , RecipeDiffEntry {
        rdeOld = Nothing,
        rdeNew = Just Package {rdtPackage = RecipeModule {rmName = "vim", rmVersion = "8.0.*"}}
      }
    , RecipeDiffEntry {
        rdeOld = Just Package {rdtPackage = RecipeModule {rmName = "bash", rmVersion = "1.0"}},
        rdeNew = Just Package {rdtPackage = RecipeModule {rmName = "bash", rmVersion = "4.4.*"}}
      }
    ]

exampleDiffJSON :: BSL.ByteString
exampleDiffJSON = [s|[
{"old": {"Description": "This is the old description"},
 "new": {"Description": "The New and Improved description"}},
{"old": {"Module": {"name": "tmux", "version": "2.2"}},
 "new": null},
{"old": null,
 "new": {"Package": {"name": "vim", "version": "8.0.*"}}},
{"old": {"Package": {"name": "bash", "version": "1.0"}},
 "new": {"Package": {"name": "bash", "version": "4.4.*"}}}
]|]

main :: IO ()
main = hspec spec


spec :: Spec
spec =
    describe "RecipeDiff JSON tests" $ do
        it "Convert List of RecipeDiffEntry records to JSON" $
            toJSON exampleDiff `shouldBe` fromJust (decode exampleDiffJSON)

        it "Convert JSON List of RecipeDiffEntry JSON to Records" $
            fromJSON (fromJust $ decode exampleDiffJSON) `shouldBe` Success exampleDiff
