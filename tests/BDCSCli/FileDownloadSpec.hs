-- Copyright (C) 2018 Red Hat, Inc.
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
module BDCSCli.FileDownloadSpec(main, spec)
  where

import           Test.Hspec

import           BDCSCli.FileDownload(getFilename)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "File Download functions" $ do
        it "Parse a good Content-Disposition header" $
             getFilename (Just "attachment; filename=homers-resume.odf;") `shouldBe` Just "homers-resume.odf"

        it "Parse an empty filename Content-Disposition header" $
             getFilename (Just "attachment; filename=") `shouldBe` Nothing

        it "Parse a bad Content-Disposition header" $
             getFilename (Just "filename=no-the-file-you-were-looking-for.txt") `shouldBe` Nothing

        it "Parse another bad Content-Disposition header" $
             getFilename (Just "looks; nothing; like; the; header;") `shouldBe` Nothing

        it "Parse no header" $
             getFilename Nothing `shouldBe` Nothing

        it "Parse an empty header" $
             getFilename (Just "") `shouldBe` Nothing
