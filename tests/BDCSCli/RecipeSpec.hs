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
module BDCSCli.RecipeSpec(main, spec)
  where

import qualified Data.Text as T
import           Test.Hspec

import           BDCSCli.Recipe

-- XXX Is this a reliable way to reference the test recipes?
getExampleToml :: IO T.Text
getExampleToml = T.pack <$> readFile "./tests/recipes/http-server.toml"

exampleRecipe :: Recipe
exampleRecipe =
    Recipe {rName = "http-server",
            rVersion = Just "0.2.0",
            rDescription = "An example http server with PHP and MySQL support.",
            rPackages = [RecipeModule {rmName = "tmux", rmVersion = "2.2"},
                         RecipeModule {rmName = "openssh-server", rmVersion = "6.6.*"},
                         RecipeModule {rmName = "rsync", rmVersion = "3.0.*"}],
            rModules = [RecipeModule {rmName = "httpd", rmVersion = "2.4.*"},
                        RecipeModule {rmName = "mod_auth_kerb", rmVersion = "5.4"},
                        RecipeModule {rmName = "mod_ssl", rmVersion = "2.4.*"},
                        RecipeModule {rmName = "php", rmVersion = "5.4.*"},
                        RecipeModule {rmName = "php-mysql", rmVersion = "5.4.*"}]
    }

main :: IO ()
main = hspec spec


spec :: Spec
spec = do
    describe "TOML Recipe Functions" $ do
        it "Convert TOML string to Recipe Record" $ do
            example_toml <- getExampleToml
            parseRecipe example_toml `shouldBe` Right exampleRecipe

        it "Convert Recipe Record to TOML string" $ do
            example_toml <- getExampleToml
            recipeTOML exampleRecipe `shouldBe` example_toml

    describe "Git Repository Functions" $
        it "Test Git Functions all in one batch" $
            runGitRepoTests `shouldReturn` True
