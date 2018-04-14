-- Copyright (C) 2018 Red Hat, Inc.
--
-- This file is part of bdcs-api.
--
-- bdcs-api is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- bdcs-api is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with bdcs-api.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE OverloadedStrings #-}

module BDCSCli.API.Types.QueueStatus(
    QueueStatus(..),
    queueStatusEnded,
    queueStatusFromText,
    queueStatusInProgress,
    queueStatusText)
  where

import           Control.Monad(mzero)
import           Data.Aeson(FromJSON(..), ToJSON(..), Value(..))
import           Data.String.ToString
import qualified Data.Text as T
import           Text.Printf

-- | What is the state of a compose in the queue?
data QueueStatus = QWaiting             -- ^ The compose has not yet started.
                 | QRunning             -- ^ The compose is in progress.
                 | QFinished            -- ^ The compose finished successfully.
                 | QFailed              -- ^ The compose finished unsuccessfully.
 deriving(Eq, Show)

instance ToJSON QueueStatus where
    toJSON QWaiting  = "WAITING"
    toJSON QRunning  = "RUNNING"
    toJSON QFinished = "FINISHED"
    toJSON QFailed   = "FAILED"

instance FromJSON QueueStatus where
    parseJSON (String s) = case queueStatusFromText s of
                               Just qs -> pure qs
                               Nothing -> mzero
    parseJSON _          = mzero

queueStatusEnded :: QueueStatus -> Bool
queueStatusEnded QFinished = True
queueStatusEnded QFailed   = True
queueStatusEnded _         = False

queueStatusFromText :: T.Text -> Maybe QueueStatus
queueStatusFromText t = case T.strip t of
    "WAITING"  -> Just QWaiting
    "RUNNING"  -> Just QRunning
    "FINISHED" -> Just QFinished
    "FAILED"   -> Just QFailed
    _          -> Nothing

queueStatusInProgress :: QueueStatus -> Bool
queueStatusInProgress QRunning = True
queueStatusInProgress _        = False

instance ToString QueueStatus where
    toString c = queueStatusText c

instance PrintfArg QueueStatus where
    formatArg x fmt | fmtChar (vFmt 's' fmt) == 's' =
        formatString (queueStatusText x) (fmt { fmtChar = 's', fmtPrecision = Nothing })
    formatArg _ fmt = errorBadFormat $ fmtChar fmt

queueStatusText :: QueueStatus -> String
queueStatusText QWaiting  = "WAITING"
queueStatusText QRunning  = "RUNNING"
queueStatusText QFinished = "FINISHED"
queueStatusText QFailed   = "FAILED"
