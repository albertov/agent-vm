{-# LANGUAGE OverloadedStrings #-}

-- | Tests for VM cache functionality
module AgentVM.VMCacheSpec (spec) where

import AgentVM.VMCache
import qualified Data.Map.Strict as Map
import Protolude
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import Test.Hspec
import UnliftIO (tryAny)

spec :: Spec
spec = describe "VMCache" $
  around_ (\action -> cleanupTestCache (Just "/tmp/agent-vm-test") >> action) $ do
    let testStateDir = Just "/tmp/agent-vm-test"
        testWorkspace1 = "/home/user/project1"
        testWorkspace2 = "/home/user/project2"
        testVmName1 = "test-vm-1"
        testVmName2 = "test-vm-2"
    describe "addVMToCache and lookupVMByWorkspace" $ do
      it "should add a VM to the cache and retrieve it" $ do
        -- Add a VM to the cache
        addVMToCache testStateDir testWorkspace1 testVmName1

        -- Look it up
        result <- lookupVMByWorkspace testStateDir testWorkspace1
        result `shouldBe` Just testVmName1

      it "should return Nothing for non-existent workspace" $ do
        result <- lookupVMByWorkspace testStateDir "/non/existent/path"
        result `shouldBe` Nothing

      it "should handle multiple VMs" $ do
        -- Add multiple VMs
        addVMToCache testStateDir testWorkspace1 testVmName1
        addVMToCache testStateDir testWorkspace2 testVmName2

        -- Look them up
        result1 <- lookupVMByWorkspace testStateDir testWorkspace1
        result2 <- lookupVMByWorkspace testStateDir testWorkspace2

        result1 `shouldBe` Just testVmName1
        result2 `shouldBe` Just testVmName2

      it "should update existing workspace mapping" $ do
        -- Add initial mapping
        addVMToCache testStateDir testWorkspace1 testVmName1

        -- Update with new VM name
        addVMToCache testStateDir testWorkspace1 testVmName2

        -- Should get the updated name
        result <- lookupVMByWorkspace testStateDir testWorkspace1
        result `shouldBe` Just testVmName2

    describe "removeVMFromCache" $ do
      it "should remove a VM from the cache" $ do
        -- Add and then remove
        addVMToCache testStateDir testWorkspace1 testVmName1
        removeVMFromCache testStateDir testWorkspace1

        -- Should not find it
        result <- lookupVMByWorkspace testStateDir testWorkspace1
        result `shouldBe` Nothing

      it "should not affect other VMs" $ do
        -- Add two VMs
        addVMToCache testStateDir testWorkspace1 testVmName1
        addVMToCache testStateDir testWorkspace2 testVmName2

        -- Remove only one
        removeVMFromCache testStateDir testWorkspace1

        -- First should be gone, second should remain
        result1 <- lookupVMByWorkspace testStateDir testWorkspace1
        result2 <- lookupVMByWorkspace testStateDir testWorkspace2

        result1 `shouldBe` Nothing
        result2 `shouldBe` Just testVmName2

    describe "file persistence" $ do
      it "should persist cache to disk" $ do
        -- Add a VM
        addVMToCache testStateDir testWorkspace1 testVmName1

        -- Check file exists
        cacheFile <- getVMCacheFile testStateDir
        exists <- doesFileExist cacheFile
        exists `shouldBe` True

      it "should load cache from disk" $ do
        -- Add a VM
        addVMToCache testStateDir testWorkspace1 testVmName1

        -- Load fresh cache (simulating new process)
        freshCache <- loadVMCache testStateDir
        let VMCache cacheMap = freshCache

        Map.lookup testWorkspace1 cacheMap `shouldBe` Just testVmName1

    describe "missing cache handling" $ do
      it "should return empty cache when file doesn't exist" $ do
        -- Ensure cache doesn't exist
        cleanupTestCache testStateDir

        -- Load should return empty cache
        cache <- loadVMCache testStateDir
        let VMCache cacheMap = cache
        Map.null cacheMap `shouldBe` True

      it "should return Nothing for lookups when cache doesn't exist" $ do
        -- Ensure cache doesn't exist
        cleanupTestCache testStateDir

        -- Lookup should return Nothing
        result <- lookupVMByWorkspace testStateDir testWorkspace1
        result `shouldBe` Nothing

      it "should create cache file on first write" $ do
        -- Ensure cache doesn't exist
        cleanupTestCache testStateDir
        cacheFile <- getVMCacheFile testStateDir
        existsBefore <- doesFileExist cacheFile
        existsBefore `shouldBe` False

        -- Add a VM (should create the file)
        addVMToCache testStateDir testWorkspace1 testVmName1

        -- File should now exist
        existsAfter <- doesFileExist cacheFile
        existsAfter `shouldBe` True

-- | Clean up test cache file
cleanupTestCache :: Maybe FilePath -> IO ()
cleanupTestCache mStateDir = do
  -- Ensure the directory exists
  for_ mStateDir (createDirectoryIfMissing True)
  cacheFile <- getVMCacheFile mStateDir
  void $ tryAny $ removeFile cacheFile
