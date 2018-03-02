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

module BDCSCli.APITypesSpec(main, spec)
  where

import           Data.Aeson(decode, toJSON)
import           Data.Maybe(fromJust)
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

main :: IO ()
main = hspec spec


spec :: Spec
spec = do
    describe "RecipeDiff to JSON tests" $ do
        it "Convert List of RecipeDiffEntry records to JSON" $ do
            toJSON exampleDiff `shouldBe` fromJust (decode "[]")
