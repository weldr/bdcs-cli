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
{-# LANGUAGE RecordWildCards   #-}

module BDCSCli.Recipe(parseRecipe,
                      doGitTests)
  where

import           Control.Conditional(ifM)
import           Control.Monad(filterM)
import           Control.Monad.Loops(allM)
import           Data.Aeson(toJSON, fromJSON)
import           Data.Aeson.Types(Result(..))
import qualified Data.ByteString as BS
import           Data.Either(rights)
import           Data.List(elemIndices, isSuffixOf)
import           Data.Maybe(fromJust, fromMaybe, isJust)
import qualified Data.SemVer as SV
import qualified Data.Text as T
import           Data.Text.Encoding(encodeUtf8)
import           Data.Word(Word32)
import           GI.Gio
import qualified GI.Ggit as Git
import qualified GI.GLib as GLib
import           System.Directory(doesPathExist, listDirectory)
import           Text.Printf(printf)
import           Text.Read(readMaybe)
import           Text.Toml(parseTomlDoc)

import           BDCSCli.API.V0(Recipe(..), RecipeModule(..))

-- | Parse a TOML formatted string and return a Recipe
parseRecipe :: T.Text -> Either String Recipe
parseRecipe xs =
    case parseTomlDoc "" xs of
        Left err    -> Left ("Parsing TOML document failed. " ++ show err)
        Right table -> do
            let json = toJSON table
            case (fromJSON json :: Result Recipe) of
                Error err -> Left ("Converting from JSON to Recipe failed. " ++ show err)
                Success r -> Right r

-- | Convert a Recipe to a TOML string
recipeTOML :: Recipe -> T.Text
recipeTOML Recipe{..} = T.concat [nameText, versionText, descriptionText, modulesText, packagesText]
  where
    nameText = T.pack $ printf "name = \"%s\"\n" rName
    versionText = T.pack $ printf "version = \"%s\"\n" $ fromMaybe "" rVersion
    descriptionText = T.pack $ printf "description = \"%s\"\n\n" rDescription

    moduleText :: T.Text -> RecipeModule -> T.Text
    moduleText name RecipeModule{..} = T.pack $ printf "[[%s]]\nname = \"%s\"\nversion = \"%s\"\n\n" name rmName rmVersion
    packagesText = T.concat $ map (moduleText "packages") rPackages
    modulesText = T.concat $ map (moduleText "modules") rModules

-- | Convert a recipe name to a toml filename
--
-- Replaces spaces with - and append .toml
recipeTomlFilename :: String -> T.Text
recipeTomlFilename name = T.append (T.replace " " "-" (T.pack name)) ".toml"

-- | semver recipe version number bump
--
-- If neither have a version 0.0.1 is returned
-- If there is no previous version the new version is checked and returned
-- If there is no new version, but there is a previous one, bump its patch level
-- If the previous and new versions are the same, bump the patch level
-- If they are different, check and return the new version
recipeBumpVersion :: Maybe String -> Maybe String -> Either String String
recipeBumpVersion Nothing Nothing = Right "0.0.1"
-- Only a new version, make sure the new version can be parsed
recipeBumpVersion Nothing (Just new_ver) =
    case SV.fromText (T.pack new_ver) of
        Right _ -> Right new_ver
        Left  _ -> Left ("Failed to parse version: " ++ new_ver)
-- If there was a previous version and no new one, bump the patch level
recipeBumpVersion (Just prev_ver) Nothing =
    case SV.fromText (T.pack prev_ver) of
        Right version -> Right $ SV.toString $ SV.incrementPatch version
        Left _        -> Left ("Failed to parse version: " ++ prev_ver)
recipeBumpVersion (Just prev_ver) (Just new_ver)
    | prev_ver == new_ver = bumpNewVer
    | otherwise           = checkNewVer
  where
    bumpNewVer =
        case SV.fromText (T.pack new_ver) of
            Right version -> Right $ SV.toString $ SV.incrementPatch version
            Left _        -> Left ("Failed to parse version: " ++ new_ver)
    checkNewVer =
        case SV.fromText (T.pack new_ver) of
            Right _ -> Right new_ver
            Left  _ -> Left ("Failed to parse version: " ++ new_ver)

-- | Open a Git repository, or create the initial repository if one doesn't exist
openOrCreateRepo :: FilePath -> IO Git.Repository
openOrCreateRepo path = do
    gfile <- fileNewForPath path
    ifM (doesPathExist path)
        (Git.repositoryOpen gfile)
        (createWithInitialCommit gfile)
  where
    createWithInitialCommit gfile = do
        repo <- Git.repositoryInitRepository gfile True

        -- Make an empty initial commit
        sig <- Git.signatureNewNow "bdcs-cli" "user-email"
        index <- Git.repositoryGetIndex repo
        tree_id <- Git.indexWriteTree index
        tree <- fromJust <$> Git.repositoryLookupTree repo tree_id
        let ref = Just "HEAD"
        let encoding = Just "UTF-8"
        commit_id <- Git.repositoryCreateCommit repo ref sig sig encoding "Initial Recipe repository commit" tree []

        return repo

-- | Lookup the Branch name or create a new branch and return a Git.Branch
findOrCreateBranch :: Git.Repository -> T.Text -> IO Git.Branch
findOrCreateBranch repo branch = do
    mbranch <- Git.repositoryLookupBranch repo branch Git.BranchTypeLocal
    maybe createBranch return mbranch
  where
    createBranch = do
        head_ref <- Git.repositoryGetHead repo
        parent_obj <- Git.refLookup head_ref
        mbranch <- Git.repositoryCreateBranch repo branch parent_obj [Git.CreateFlagsNone]
        -- XXX Handle errors
        return $ fromJust mbranch

-- | Convert a Branch object to an OId
-- XXX Ignored error handling for the moment.
getBranchOIdFromObject :: Git.Repository -> Git.Branch -> IO Git.OId
getBranchOIdFromObject repo branch_obj = do
    branch_name <- Git.branchGetName branch_obj
    let branch_ref = T.pack $ printf "refs/heads/%s" branch_name
    mref <- Git.repositoryLookupReference repo branch_ref
    -- XXX Handle errors
    let ref = fromJust mref
    moid <- Git.refGetTarget ref
    return $ fromJust moid

-- | Make a new commit to a repository's branch
-- XXX Ignored error handling for the moment.
writeCommit :: Git.Repository -> T.Text -> T.Text -> T.Text -> BS.ByteString -> IO Git.OId
writeCommit repo branch filename message content = do
    -- does the branch exist? If so get its OId: repositoryLookupBranch
    -- If it does not, create it and get its OId: repositoryCreateBranch
    branch_obj <- findOrCreateBranch repo branch
    branch_id <- getBranchOIdFromObject repo branch_obj

    -- get the parent commit for this branch: repositoryLookupCommit
    mparent_commit <- Git.repositoryLookupCommit repo branch_id
    -- XXX Handle errors
    let parent_commit = fromJust mparent_commit

    -- create a blob for content: repositoryCreateBlobFromBuffer
    blob_id <- Git.repositoryCreateBlobFromBuffer repo content

    -- Use treebuilder to make a new entry for this filename and blob: repositoryCreateTreeBuilderFromTree
    mparent_tree <- Git.commitGetTree parent_commit
    -- XXX Handle errors
    let parent_tree = fromJust mparent_tree
    builder <- Git.repositoryCreateTreeBuilderFromTree repo parent_tree
    -- treeBuilderInsert, treeBuilderWrite
    Git.treeBuilderInsert builder filename blob_id Git.FileModeBlob
    tree_id <- Git.treeBuilderWrite builder
    -- XXX Handle errors
    mtree <- Git.repositoryLookupTree repo tree_id
    let tree = fromJust mtree

    -- Create a signature
    sig <- Git.signatureNewNow "bdcs-cli" "user-email"

    let ref = Just $ T.pack $ printf "refs/heads/%s" branch
    let encoding = Just "UTF-8"

    -- Create a new commit: repositoryCreateCommit
    Git.repositoryCreateCommit repo ref sig sig encoding message tree [parent_commit]

-- | Read a commit and return a ByteString of the content
-- TODO Return the commit message too
readCommit :: Git.Repository -> T.Text -> T.Text -> Maybe T.Text -> IO BS.ByteString
readCommit repo branch filename Nothing = do
    let spec = T.pack $ printf "%s:%s" branch filename
    readCommitSpec repo spec
readCommit repo _ filename commit = do
    let spec = T.pack $ printf "%s:%s" (fromJust commit) filename
    readCommitSpec repo spec

-- | Read a commit usinga revspec, return the ByteString content
readCommitSpec :: Git.Repository -> T.Text -> IO BS.ByteString
readCommitSpec repo spec = do
    obj <- Git.repositoryRevparse repo spec
    oid <- Git.objectGetId obj
    -- XXX Handle errors
    mblob <- Git.repositoryLookupBlob repo oid
    let blob = fromJust mblob
    Git.blobGetRawContent blob

-- | Get the filename for a Blob tree entry
getFilename :: Git.Tree -> Word32 -> IO (Maybe T.Text)
getFilename tree idx = do
    mentry <- Git.treeGet tree idx
    -- XXX Handle errors
    let entry = fromJust mentry

    -- Only allow Blob and BlobExecutable
    isBlob <- isFileBlob entry
    if isBlob
        then do
            mfilename <- Git.treeEntryGetName entry
            -- XXX Handle errors
            let name = fromJust mfilename
            return $ Just name
        else return Nothing
 where
    isFileBlob entry = do
        mode <- Git.treeEntryGetFileMode entry
        case mode of
            Git.FileModeBlob           -> return True
            Git.FileModeBlobExecutable -> return True
            _                          -> return False

{-# ANN getFilenames ("HLint: ignore Eta reduce"::String) #-}
-- | Get a list of the Blob tree entry filenames
getFilenames :: Git.Tree -> Word32 -> IO [T.Text]
getFilenames tree idx = getFilenames' tree [] idx

-- | Build the list of filenames from the tree entries
getFilenames' :: Git.Tree -> [T.Text] -> Word32 -> IO [T.Text]
getFilenames' _ filenames 0 = return filenames
getFilenames' tree filenames idx = do
    filename <- getFilename tree (idx-1)
    case filename of
        Just name -> getFilenames' tree (name:filenames) (idx-1)
        Nothing   -> getFilenames' tree filenames (idx-1)

-- | List the files on a branch
listBranchFiles :: Git.Repository -> T.Text -> Maybe T.Text -> IO [T.Text]
listBranchFiles repo branch Nothing = do
    mbranch <- Git.repositoryLookupBranch repo branch Git.BranchTypeLocal
    -- XXX Handle errors
    let branch_obj = fromJust mbranch
    branch_id <- getBranchOIdFromObject repo branch_obj
    -- get the parent commit for this branch: repositoryLookupCommit
    mparent_commit <- Git.repositoryLookupCommit repo branch_id
    -- XXX Handle errors
    let parent_commit = fromJust mparent_commit
    listCommitFiles repo parent_commit

-- | List the files in a commit
listCommitFiles :: Git.Repository -> Git.Commit -> IO [T.Text]
listCommitFiles repo commit = do
    mparent_tree_id <- Git.commitGetTreeId commit
    let parent_tree_id = fromJust mparent_tree_id

    mtree <- Git.repositoryLookupTree repo parent_tree_id
    -- XXX Handle errors
    let tree = fromJust mtree

    sz <- Git.treeSize tree
    getFilenames tree sz

deleteFile :: Git.Repository -> T.Text -> T.Text -> IO Git.OId
deleteFile repo branch filename = do
    mbranch <- Git.repositoryLookupBranch repo branch Git.BranchTypeLocal
    -- XXX Handle errors
    let branch_obj = fromJust mbranch
    branch_id <- getBranchOIdFromObject repo branch_obj
    -- get the parent commit for this branch: repositoryLookupCommit
    mparent_commit <- Git.repositoryLookupCommit repo branch_id
    -- XXX Handle errors
    let parent_commit = fromJust mparent_commit

    -- Use treebuilder to modify the tree
    mparent_tree <- Git.commitGetTree parent_commit
    -- XXX Handle errors
    let parent_tree = fromJust mparent_tree
    builder <- Git.repositoryCreateTreeBuilderFromTree repo parent_tree
    -- treeBuilderInsert, treeBuilderWrite
    Git.treeBuilderRemove builder filename
    tree_id <- Git.treeBuilderWrite builder
    -- XXX Handle errors
    mtree <- Git.repositoryLookupTree repo tree_id
    let tree = fromJust mtree

    -- Create a signature
    sig <- Git.signatureNewNow "bdcs-cli" "user-email"

    let ref = Just $ T.pack $ printf "refs/heads/%s" branch
    let encoding = Just "UTF-8"

    let message = T.pack $ printf "Recipe %s deleted" filename

    -- Create a new commit: repositoryCreateCommit
    Git.repositoryCreateCommit repo ref sig sig encoding message tree [parent_commit]

revertFile :: Git.Repository -> T.Text -> T.Text -> T.Text -> IO Git.OId
revertFile repo branch filename commit = do
    mcommit_id <- Git.oIdNewFromString commit
    -- XXX Handle errors
    let commit_id = fromJust mcommit_id
    revertFileCommit repo branch filename commit_id

revertFileCommit :: Git.Repository -> T.Text -> T.Text -> Git.OId -> IO Git.OId
revertFileCommit repo branch filename commit_id = do
    mcommit_obj <- Git.repositoryLookupCommit repo commit_id
    -- XXX Handle errors
    let commit_obj = fromJust mcommit_obj
    mrevert_tree <- Git.commitGetTree commit_obj
    -- XXX Handle errors
    let revert_tree = fromJust mrevert_tree
    mentry <- Git.treeGetByName revert_tree filename
    -- XXX Handle errors
    let entry = fromJust mentry
-- XXX I think this is correct
    mblob_id <- Git.treeEntryGetId entry
    -- XXX Handle errors
    let blob_id = fromJust mblob_id

-- vvv This could be a function, it's used in multiple places
    mbranch <- Git.repositoryLookupBranch repo branch Git.BranchTypeLocal
    -- XXX Handle errors
    let branch_obj = fromJust mbranch
    branch_id <- getBranchOIdFromObject repo branch_obj
    -- get the parent commit for this branch: repositoryLookupCommit
    mparent_commit <- Git.repositoryLookupCommit repo branch_id
    -- XXX Handle errors
    let parent_commit = fromJust mparent_commit
-- ^^^ This could be a function, it's used in multiple places

    -- Use treebuilder to modify the tree
    mparent_tree <- Git.commitGetTree parent_commit
    -- XXX Handle errors
    let parent_tree = fromJust mparent_tree
    builder <- Git.repositoryCreateTreeBuilderFromTree repo parent_tree

    Git.treeBuilderInsert builder filename blob_id Git.FileModeBlob
    tree_id <- Git.treeBuilderWrite builder
    mtree <- Git.repositoryLookupTree repo tree_id
    -- XXX Handle errors
    let tree = fromJust mtree

    -- Create a signature
    sig <- Git.signatureNewNow "bdcs-cli" "user-email"

    let ref = Just $ T.pack $ printf "refs/heads/%s" branch
    let encoding = Just "UTF-8"

    mcommit <- Git.oIdToString commit_id
    -- XXX Handle errors
    let commit = fromJust mcommit
    let message = T.pack $ printf "Recipe %s reverted to commit %s" filename commit

    -- Create a new commit: repositoryCreateCommit
    Git.repositoryCreateCommit repo ref sig sig encoding message tree [parent_commit]

-- | File commit details
data CommitDetails =
    CommitDetails { cdCommit    :: T.Text
                  , cdTime      :: T.Text
                  , cdMessage   :: T.Text
                  , cdRevision  :: Maybe Int
    } deriving (Show, Eq)

listCommits :: Git.Repository -> T.Text -> T.Text -> IO [CommitDetails]
listCommits repo branch filename = do
    mrevwalk <- Git.revisionWalkerNew repo
    -- XXX Handle errors
    let revwalk = fromJust mrevwalk
    Git.revisionWalkerSetSortMode revwalk [Git.SortModeTime, Git.SortModeReverse]
    let branch_ref = T.pack $ printf "refs/heads/%s" branch
    Git.revisionWalkerPushRef revwalk branch_ref

    mfirst_id <- Git.revisionWalkerNext revwalk
    commitDetails repo revwalk branch filename [] mfirst_id

commitDetails :: Git.Repository -> Git.RevisionWalker -> T.Text -> T.Text -> [CommitDetails] -> Maybe Git.OId -> IO [CommitDetails]
commitDetails _ _ _ _ details Nothing = return details
commitDetails repo revwalk branch filename details next_id = do
    let commit_id = fromJust next_id
    mcommit_obj <- Git.repositoryLookupCommit repo commit_id
    -- XXX Handle errors
    let commit_obj = fromJust mcommit_obj

    mparents <- Git.commitGetParents commit_obj
    -- XXX Handle errors
    let parents = fromJust mparents
    num_parents <- Git.commitParentsGetSize parents

    mtree <- Git.commitGetTree commit_obj
    -- XXX Handle errors
    let tree = fromJust mtree

    is_diff <- if num_parents > 0
        then do
            commits <- mapM (Git.commitParentsGet parents) [0..num_parents-1]
            allM (parentDiff repo filename tree) commits
        else
            return False

    mnext_id <- Git.revisionWalkerNext revwalk
    mentry <- Git.treeGetByName tree filename
    if isJust mentry && is_diff
        then getCommitDetails commit_id commit_obj mnext_id
        else commitDetails repo revwalk branch filename details mnext_id

  where
    getCommitDetails :: Git.OId -> Git.Commit -> Maybe Git.OId -> IO [CommitDetails]
    getCommitDetails commit_id commit_obj mnext_id = do
        mtag <- findCommitTag repo branch filename commit_id
        let revision = getRevisionFromTag mtag
        -- Fill in a commit record
        message <- Git.commitGetMessage commit_obj
        mid <- Git.oIdToString commit_id
        -- XXX How could that fail?
        let commit_str = fromJust mid
        sig <- Git.commitGetCommitter commit_obj

        -- XXX No Idea How To Convert These Yet
        datetime <- Git.signatureGetTime sig
        timezone <- Git.signatureGetTimeZone sig
        -- What do you do with the TimeZone?
        timeval <- GLib.newZeroTimeVal
        ok <- GLib.dateTimeToTimeval datetime timeval
        -- XXX Handle error (ok == False)
        time_str <- GLib.timeValToIso8601 timeval

        let commit = CommitDetails {cdCommit=commit_str, cdTime=time_str, cdMessage=message, cdRevision=revision}
        commitDetails repo revwalk branch filename (commit:details) mnext_id

-- | Determine if there were changes between this commit and its parent
--
-- Return True if there were changes, False otherwise
parentDiff :: Git.Repository -> T.Text -> Git.Tree -> Git.Commit -> IO Bool
parentDiff repo filename commit_tree parent_commit = do
    mdiff_opts <- Git.diffOptionsNew
    -- XXX Handle errors
    let diff_opts = fromJust mdiff_opts
    Git.diffOptionsSetPathspec diff_opts (Just [filename])

    mparent_tree <- Git.commitGetTree parent_commit
    -- XXX Handle errors
    let parent_tree = fromJust mparent_tree
    mdiff <- Git.diffNewTreeToTree repo (Just commit_tree) (Just parent_tree) (Just diff_opts)
    let diff = fromJust mdiff
    num_deltas <- Git.diffGetNumDeltas diff
    if num_deltas > 0
    then return True
    else return False

-- | Find the revision tag pointing to a specific commit
--
-- Tag is of the form 'refs/tags/<branch>/<filename>/r<revision>
-- There should really only be one.
findCommitTag :: Git.Repository -> T.Text -> T.Text -> Git.OId -> IO (Maybe T.Text)
findCommitTag repo branch filename commit_id = do
    let tag_pattern = T.pack $ printf "%s/%s/r*" branch filename
    mall_tags <- Git.repositoryListTagsMatch repo (Just tag_pattern)
    case mall_tags of
        Just []    -> return Nothing
        Just tags  -> filterTags tags
        Nothing    -> return Nothing
  where
    filterTags tags = do
        commit_tags <- filterM isCommitTag tags
        return $ maybeOneTag commit_tags

    maybeOneTag :: [T.Text] -> Maybe T.Text
    maybeOneTag []    = Nothing
    maybeOneTag [tag] = Just tag
    maybeOneTag _     = Nothing

    -- | Return True if the tag is on the commit
    isCommitTag :: T.Text -> IO Bool
    isCommitTag tag = do
        -- Find the commit for this tag and check that it matches commit_id
        -- If so, return the branch/filename/r* part of the tag
        let ref_tag = T.pack $ printf "refs/tags/%s" tag
        -- XXX This dumb thing will throw an exception if the reference is wrong
        mref <- Git.repositoryLookupReference repo ref_tag
        -- XXX Handle errors
        let ref = fromJust mref
        mtag_oid <- Git.refGetTarget ref
        -- XXX Handle errors
        let tag_oid = fromJust mtag_oid
        mtag_obj <- Git.repositoryLookupTag repo tag_oid
        -- XXX Handle errors
        let tag_obj = fromJust mtag_obj
        moid <- Git.tagGetTargetId tag_obj
        -- XXX Handle errors
        let oid = fromJust moid

        cmp <- Git.oIdCompare oid commit_id
        if cmp == 0
            then return True
            else return False

-- | Return the revision number from a git tag
-- Tag is of the form 'refs/tags/<branch>/<filename>/r<revision>
--
-- Returns a Just Int if all goes well, otherwise Nothing
getRevisionFromTag :: Maybe T.Text -> Maybe Int
getRevisionFromTag mtag = case mtag of
    Nothing  -> Nothing
    Just tag -> getRevision $ T.unpack tag
  where
    getRevision :: String -> Maybe Int
    getRevision tag = do
        -- Get the digits after the final r
        let rs = elemIndices 'r' tag
        if null rs
            then Nothing
            else readMaybe $ drop (last rs + 1) tag

-- | Tag a file's most recent commit
--
-- This uses git tags, of the form `refs/tags/<branch>/<filename>/r<revision>`
-- Only the most recent recipe commit can be tagged to prevent out of order tagging.
-- Revisions start at 1 and increment for each new commit that is tagged.
-- If the commit has already been tagged it will return false.
tagFileCommit :: Git.Repository -> T.Text -> T.Text -> IO Bool
tagFileCommit repo branch filename = do
    commits <- listCommits repo branch filename
    let rev_commit = findLastRev commits
    -- If there are no commits, or the most recent one has already been tagged, return False
    if null commits || isFirstCommit commits rev_commit
        then return False
        else tagNewestCommit (head commits) rev_commit
  where
    -- | Tag the most recent commit
    tagNewestCommit :: CommitDetails -> Maybe CommitDetails -> IO Bool
    tagNewestCommit last_commit rev_commit = do
        -- What revision is this? rev_commit may be Nothing, or cdRevision may be Nothing. Use 1 for those cases
        let rev = if isJust rev_commit && isJust (cdRevision (fromJust rev_commit))
                  then fromJust (cdRevision (fromJust rev_commit)) + 1
                  else 1

        let name = T.pack $ printf "%s/%s/r%d" branch filename rev
        sig <- Git.signatureNewNow "bdcs-cli" "user-email"
        mcommit_id <- Git.oIdNewFromString (cdCommit last_commit)
        -- XXX Handle errors
        let commit_id = fromJust mcommit_id
        -- XXX don't know how to create a GType here...
        commit_type <- gobjectType (undefined :: Git.Commit)
        commit_obj <- Git.repositoryLookup repo commit_id commit_type
        mtag_id <- Git.repositoryCreateTag repo name commit_obj sig name [Git.CreateFlagsNone]
        if isJust mtag_id
            then return True
            else return False

    -- | Find the last revision in the commits and return it
    findLastRev :: [CommitDetails] -> Maybe CommitDetails
    findLastRev []= Nothing
    findLastRev (x:xs) = case cdRevision x of
                             Nothing  -> findLastRev xs
                             Just _   -> Just x

    -- | Is the revision commit the most recent one?
    --
    -- If it is, then we cannot make a new tag.
    -- If it is not, or there is no rev_commit, we can tag a new one.
    isFirstCommit :: [CommitDetails] -> Maybe CommitDetails -> Bool
    isFirstCommit commits rev_commit = case rev_commit of
                                           Nothing -> False
                                           Just commit -> commit == head commits


-- | Read and parse a recipe file
commitRecipeFile :: Git.Repository -> T.Text -> FilePath -> IO Git.OId
commitRecipeFile repo branch filename = do
    toml_in <- readFile filename
    let erecipe = parseRecipe (T.pack toml_in)
    -- XXX Handle errors
    let recipe = head $ rights [erecipe]
    commitRecipe repo branch recipe

-- | Commit a Recipe record to a branch
commitRecipe :: Git.Repository -> T.Text -> Recipe -> IO Git.OId
commitRecipe repo branch recipe = do
    let toml_out = encodeUtf8 $ recipeTOML recipe
    let filename = recipeTomlFilename (rName recipe)
    let eversion = recipeBumpVersion Nothing (rVersion recipe)
    -- XXX Handle errors
    let version = head $ rights [eversion]
    let message = T.pack $ printf "Recipe %s, version %s saved" filename version
    writeCommit repo branch filename message toml_out

-- | Commit recipes from a directory, if they don't already exist
--
commitRecipeDirectory :: Git.Repository -> T.Text -> FilePath -> IO [Git.OId]
commitRecipeDirectory repo branch directory = do
    branch_files <- listBranchFiles repo branch Nothing
    files <- map (directory ++) . filter (skipFiles branch_files) <$> listDirectory directory
    mapM (commitRecipeFile repo branch) files
  where
    skipFiles :: [T.Text] -> String -> Bool
    skipFiles branch_files file = T.pack file `notElem` branch_files && ".toml" `isSuffixOf` file

printOId :: Git.OId -> IO ()
printOId oid = do
    moid_str <- Git.oIdToString oid
    print moid_str

-- | Test out git functions
doGitTests :: FilePath -> IO ()
doGitTests path = do
    -- Move this elsewhere later, MUST be called first
    Git.init
    repo <- openOrCreateRepo path
-- WORKS
    commit_id <- writeCommit repo "master" "README" "A test commit\n\nWith some commentary." "Some Content"
    commit_id <- writeCommit repo "master" "README" "A test commit 2\n\nWith some other commentary." "Some Content #2"
    commit_id <- writeCommit repo "master" "TODO" "A todo commit\n\nWith some stuff to do." "Some Content"
    commit_id <- writeCommit repo "master" "TODO" "A todo commit\n\nWith some commentary." "Some other Content"

    files <- listBranchFiles repo "master" Nothing
    print files

    commit_id <- writeCommit repo "master" "FRODO" "A list of stuff to be accomplished\n\nFind a magic ring" "Some other Content"
    files <- listBranchFiles repo "master" Nothing
    print files

    deleteFile repo "master" "FRODO"

    files <- listBranchFiles repo "master" Nothing
    print files

    -- Revert the delete
    revertFileCommit repo "master" "FRODO" commit_id

    files <- listBranchFiles repo "master" Nothing
    print files

    commits <- listCommits repo "master" "FRODO"
    print commits

    commits <- listCommits repo "master" "TODO"
    print commits

    -- First is True, 2nd is False (because there are no new commits)
    ok <- tagFileCommit repo "master" "FRODO"
    print ok
    ok <- tagFileCommit repo "master" "FRODO"
    print ok

    commitRecipeDirectory repo "master" "/home/bcl/tmp/recipes/"

    return ()
