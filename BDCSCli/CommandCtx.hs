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

module BDCSCli.CommandCtx(CommandCtx(..))
  where

import Network.Wreq.Session as S

import BDCSCli.Cmdline(CliOptions(..))

-- | Session and cmdline options for use by the application
data CommandCtx = CommandCtx {
    ctxSession :: Session,
    ctxOptions :: CliOptions
}
