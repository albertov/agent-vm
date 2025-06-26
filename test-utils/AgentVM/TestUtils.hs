{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Test utilities for AgentVM testing
module AgentVM.TestUtils
  ( withTestEnv,
    parseLogLevel, -- Export for testing
  )
where

import AgentVM.Env (AgentVmEnv (..))
import AgentVM.Log (AgentVmTrace, LogLevel (..), renderTracedMessage, traceLevel)
import Plow.Logging (IOTracer (IOTracer), Tracer (Tracer), filterTracer)
import Plow.Logging.Async (withAsyncHandleTracer)
import Protolude
import System.Environment (lookupEnv)
import UnliftIO (MonadUnliftIO, withSystemTempDirectory)
import UnliftIO.IORef (IORef, atomicModifyIORef', newIORef)

-- | Parse LogLevel from string using Read instance
parseLogLevel :: Text -> Maybe LogLevel
parseLogLevel = readMaybe

withTestEnv ::
  (MonadUnliftIO m) =>
  ((AgentVmEnv, IORef [AgentVmTrace]) -> m a) ->
  m a
withTestEnv fun = do
  -- Read minimum log level from environment variable
  minLevelEnv <- liftIO $ lookupEnv "AGENT_VM_LOGLEVEL"
  minLevel <- case minLevelEnv of
    Just levelStr -> case readMaybe levelStr of
      Just logLevel -> pure logLevel
      Nothing -> liftIO $ die $ "Invalid logLevel: " <> toS levelStr
    Nothing -> pure Info
  withAsyncHandleTracer stderr 1000 $ \asyncTracer ->
    withSystemTempDirectory "agent-vm-tests-XXXXXXX" $ \stateDir -> do
      tracesRef <- newIORef []
      let refTracer :: forall m. (MonadIO m) => Tracer m AgentVmTrace
          refTracer =
            Tracer $
              \traceEvent -> liftIO $ atomicModifyIORef' tracesRef ((,()) . (traceEvent :))

          vmTracer :: forall m. (MonadIO m) => Tracer m AgentVmTrace
          vmTracer = filterTracer (\traceEvent -> traceLevel traceEvent >= minLevel) $
            Tracer $ \traceEvent -> do
              let IOTracer (Tracer textTracerFunc) = asyncTracer
              textTracerFunc (renderTracedMessage traceEvent)

          tracer = IOTracer $ refTracer <> vmTracer
      fun (AgentVmEnv {tracer, stateDir}, tracesRef)
