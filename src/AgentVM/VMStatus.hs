{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | VM status information and utilities
module AgentVM.VMStatus
  ( -- * Status retrieval
    getDetailedVMStatus,
    getMemoryInfo,
    getCPUInfo,

    -- * Rendering
    renderVMStatus,
    renderMemoryInfo,
    renderCPUInfo,
    formatMemoryBytes,
    formatCPUTime,
  )
where

import AgentVM.Git (GitInfo (..), getGitInfo)
import AgentVM.Types (CPUInfo (..), MemoryInfo (..), VMConfig, VMStatus (..), workspace)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime)
import Protolude hiding (throwIO)
import Text.Printf (printf)
import UnliftIO (catchAny)

-- | Get detailed VM status including memory, CPU usage, and git info
-- Only called when we know the VM is running and has a valid PID
getDetailedVMStatus :: VMConfig -> Int -> IO VMStatus
getDetailedVMStatus config pid = do
  memInfo <- getMemoryInfo pid
  cpuInfo <- getCPUInfo pid
  gitInfo <- getGitInfo (workspace config)
  pure $ VMStatus pid memInfo cpuInfo gitInfo

-- | Get memory information from /proc filesystem
getMemoryInfo :: Int -> IO (Maybe MemoryInfo)
getMemoryInfo pid = do
  let statusPath = "/proc/" <> show pid <> "/status"

  statusInfo <- catchAny (readFileLines statusPath) (\_ -> pure [])

  let extractAndParseField prefix = do
        line <- find (T.isPrefixOf prefix) statusInfo
        parseMemoryLine line

  if null statusInfo
    then pure Nothing
    else
      pure $
        Just $
          MemoryInfo
            (extractAndParseField "VmPeak:")
            (extractAndParseField "VmSize:")
            (extractAndParseField "VmRSS:")

-- | Parse memory line from /proc/[pid]/status and convert kB to bytes
parseMemoryLine :: Text -> Maybe Int64
parseMemoryLine line =
  case T.words line of
    (_ : valueStr : "kB" : _) ->
      case readMaybe (T.unpack valueStr) of
        Just (kbValue :: Int64) -> Just (kbValue * 1024) -- Convert kB to bytes
        Nothing -> Nothing
    _ -> Nothing

-- | Get CPU information from /proc filesystem
getCPUInfo :: Int -> IO (Maybe CPUInfo)
getCPUInfo pid = do
  let statPath = "/proc/" <> show pid <> "/stat"

  statInfo <- catchAny (readFile statPath) (\_ -> pure "")

  case T.words (toS statInfo) of
    fields
      | length fields > 14 -> do
          let utimeStr = case drop 13 fields of
                x : _ -> x
                [] -> "0"
              stimeStr = case drop 14 fields of
                x : _ -> x
                [] -> "0"

          case (readMaybe (T.unpack utimeStr), readMaybe (T.unpack stimeStr)) of
            (Just (utime :: Int64), Just (stime :: Int64)) -> do
              -- Convert clock ticks to seconds (typical USER_HZ is 100)
              let clockTicksPerSec = 100 :: Double
                  utimeSeconds = fromIntegral utime / clockTicksPerSec
                  stimeSeconds = fromIntegral stime / clockTicksPerSec
              pure $
                Just $
                  CPUInfo
                    (realToFrac utimeSeconds)
                    (realToFrac stimeSeconds)
            _ -> pure Nothing
    _ -> pure Nothing

-- | Read file and split into lines
readFileLines :: FilePath -> IO [Text]
readFileLines path = T.lines . toS <$> readFile path

-- | Render VMStatus to list of text lines
renderVMStatus :: VMStatus -> [Text]
renderVMStatus vmStatus =
  let pidInfo = ["Process ID: " <> toS (show (vmStatusPid vmStatus) :: [Char])]
      memInfo = maybe [] renderMemoryInfo (vmStatusMemoryInfo vmStatus)
      cpuInfo = maybe [] renderCPUInfo (vmStatusCPUInfo vmStatus)
      gitInfo = maybe ["No git repository or no commits"] renderGitInfo (vmStatusGitInfo vmStatus)
   in pidInfo ++ memInfo ++ cpuInfo ++ gitInfo

-- | Render memory information
renderMemoryInfo :: MemoryInfo -> [Text]
renderMemoryInfo memInfo =
  catMaybes
    [ fmap (formatMemoryBytes "VmPeak:") (memVmPeak memInfo),
      fmap (formatMemoryBytes "VmSize:") (memVmSize memInfo),
      fmap (formatMemoryBytes "VmRSS:") (memVmRSS memInfo)
    ]

-- | Format memory bytes to MB/GB with 2 decimals
formatMemoryBytes :: Text -> Int64 -> Text
formatMemoryBytes fieldName bytes =
  let mbValue = fromIntegral bytes / (1024 * 1024) :: Double
      gbValue = mbValue / 1024
      formatted =
        if gbValue >= 1.0
          then T.pack (printf "%.2f GB" gbValue)
          else T.pack (printf "%.2f MB" mbValue)
   in fieldName <> " " <> formatted

-- | Render CPU information
renderCPUInfo :: CPUInfo -> [Text]
renderCPUInfo cpuInfo =
  [ "CPU User Time: " <> formatCPUTime (cpuUserTime cpuInfo),
    "CPU System Time: " <> formatCPUTime (cpuSystemTime cpuInfo)
  ]

-- | Format NominalDiffTime as human-readable text
formatCPUTime :: NominalDiffTime -> Text
formatCPUTime time =
  let seconds = realToFrac time :: Double
   in T.pack (printf "%.2f seconds" seconds)

-- | Render git information
renderGitInfo :: GitInfo -> [Text]
renderGitInfo gitInfo =
  [ "Last commit diff:",
    gitLastCommitDiff gitInfo
  ]
