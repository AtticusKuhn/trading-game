{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ConcurrentTests (concurrentProperties) where

import qualified Control.Concurrent.Async as Async
import Control.Concurrent.MVar
import Control.Effect (Eff, IOE, liftIO, runIO)
import qualified Control.Effect.Error as Error
import Control.Exception (SomeException, displayException, finally, fromException, throwIO, try)
import Data.List (isInfixOf)
import Test.QuickCheck
import LiveTests (liveProperty)
import TradingGame.Concurrent

concurrentProperties :: [Property]
concurrentProperties =
  [ property prop_scopeCleanup
  , property prop_workerFailure
  , property prop_parentCancellation
  , property prop_nestedScopes
  , once prop_controlBoundary
  ]

-- A successful body cancels workers, even while they are blocked in IO.
prop_scopeCleanup :: Int -> Property
prop_scopeCleanup value = liveProperty "scoped worker cleanup" $ do
  started <- newEmptyMVar
  stopped <- newEmptyMVar
  blocker <- newEmptyMVar
  result <- runIO $ runConcurrent $ withWorkers
    [liftIO ((putMVar started () >> takeMVar blocker) `finally` putMVar stopped ())]
    (liftIO (takeMVar started) >> pure value)
  cleaned <- tryTakeMVar stopped
  pure (conjoin [result === value, cleaned === Just ()])

-- Worker exceptions must terminate a blocked body and clean up its siblings.
prop_workerFailure :: Int -> Property
prop_workerFailure value = liveProperty "worker exception supervision" $ do
  siblingStarted <- newEmptyMVar
  bodyStarted <- newEmptyMVar
  siblingStopped <- newEmptyMVar
  bodyStopped <- newEmptyMVar
  blocker <- newEmptyMVar
  let message = "worker failed: " ++ show value
      sibling = liftIO ((putMVar siblingStarted () >> takeMVar blocker)
        `finally` putMVar siblingStopped ())
      failure = liftIO $ takeMVar siblingStarted >> takeMVar bodyStarted >> throwIO (userError message)
      body = liftIO ((putMVar bodyStarted () >> takeMVar blocker) `finally` putMVar bodyStopped ())
  result <- try (runIO $ runConcurrent $ withWorkers [sibling, failure] body)
  siblingCleaned <- tryTakeMVar siblingStopped
  bodyCleaned <- tryTakeMVar bodyStopped
  pure $ conjoin
    [ property (either (isFailure message) (const False) result)
    , siblingCleaned === Just (), bodyCleaned === Just ()
    ]

isFailure :: String -> SomeException -> Bool
isFailure message err = case fromException err of
  Just (Async.ExceptionInLinkedThread _ cause) -> isFailure message cause
  Nothing -> fromException err == Just (userError message)

prop_parentCancellation :: Int -> Property
prop_parentCancellation value = liveProperty "parent cancellation cleans workers" $ do
  started <- newEmptyMVar
  stopped <- newEmptyMVar
  blocker <- newEmptyMVar
  let worker = liftIO ((putMVar started () >> takeMVar blocker) `finally` putMVar stopped value)
      action = runIO $ runConcurrent $ withWorkers [worker] (liftIO (takeMVar blocker))
  Async.withAsync action $ \parent -> do
    takeMVar started
    Async.cancel parent
    cleaned <- tryTakeMVar stopped
    pure (cleaned === Just value)

prop_nestedScopes :: Int -> Property
prop_nestedScopes value = liveProperty "nested and empty worker scopes" $ do
  delivered <- newEmptyMVar
  let localError = Error.runError (Error.throw value) :: Eff '[Concurrent, IOE] (Either Int ())
  result <- runIO $ runConcurrent $ withWorkers
    [withWorkers [] (localError >>= liftIO . putMVar delivered)]
    (liftIO (takeMVar delivered))
  pure (result === Left value)

-- Cross-thread control must fail explicitly, never escape as an RTS prompt
-- failure. Handlers installed inside a worker are covered by nestedScopes.
prop_controlBoundary :: Property
prop_controlBoundary = liveProperty "explicit nonlocal control boundary" $ do
  blocker <- newEmptyMVar
  let action = runIO $ Error.runError $ runConcurrent $
        withWorkers [Error.throw ("outside" :: String)] (liftIO (takeMVar blocker))
  result <- try action :: IO (Either SomeException (Either String ()))
  pure $ property $ case result of
    Left err -> "nonlocal effect control" `isInfixOf` displayException err
    Right _ -> False
