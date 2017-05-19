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
{-# LANGUAGE ScopedTypeVariables #-}

module BDCSCli.Utilities(argify,
                         join,
                         maybeIO)
  where

import qualified Control.Exception as E
import Control.Monad (liftM)
import Data.List (intersperse)
import Data.List.Split (splitOn)

-- | Turn exceptions from an action into Nothing
maybeIO :: IO a -> IO (Maybe a)
maybeIO act = E.handle (\(_::E.SomeException) -> (return Nothing)) (Just `liftM` act)

-- | Join a list of strings with a delimiter.
join :: [a] -> [[a]] -> [a]
join delim xs = concat (intersperse delim xs)

-- | Take a list of possiby comma, or comma-space, separated options and turn it into a list of options
argify :: Foldable t => t [Char] -> [[Char]]
argify xs = filter (/= "") $ concatMap (splitOn ",") xs
