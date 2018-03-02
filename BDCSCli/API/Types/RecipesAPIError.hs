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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module BDCSCli.API.Types.RecipesAPIError(
    RecipesAPIError(..)
) where

import           Data.Aeson

-- | RecipesAPIError is used to report errors with the /recipes/ routes
--
-- This is converted to a JSON error response that is used in the API responses
--
-- > {
-- >     "recipe": "unknown-recipe",
-- >     "msg": "unknown-recipe.toml is not present on branch master"
-- > }
data RecipesAPIError = RecipesAPIError
    { raeRecipe  :: String
    , raeMsg     :: String
    } deriving (Eq, Show)

instance FromJSON RecipesAPIError where
  parseJSON = withObject "API Error" $ \o -> do
    raeRecipe <- o .: "recipe"
    raeMsg    <- o .: "msg"
    return RecipesAPIError{..}

instance ToJSON RecipesAPIError where
  toJSON RecipesAPIError{..} = object
    [ "recipe".= raeRecipe
    , "msg"   .= raeMsg
    ]
