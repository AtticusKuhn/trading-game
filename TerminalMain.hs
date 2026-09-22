{-# LANGUAGE DataKinds #-}

module Main where

import Control.Effect (liftIO, runIO)
import Control.Monad (void)
import Data.IORef
import System.Environment (getArgs)
import System.Exit (die)
import Text.Read (readMaybe)
import TradingGame
import TradingGame.Terminal

-- The host fixes the roster before anyone joins. Joining never allocates a
-- private number or account. These names are selectors, not authentication.
roster :: [Player]
roster = [Player (PlayerId 1) "alice" 3, Player (PlayerId 2) "market-maker" 7]

-- This terminal demo has one active caller and a passive book. In sim mode
-- alarms advance its virtual clock immediately; no background timer runs.
-- General multi-program simulations still use runTradingGame's event queue.
terminalSimulationClock :: IO LiveClock
terminalSimulationClock = do
  time <- newIORef simulationStart
  pure $ LiveClock (readIORef time) $ \target -> do
    atomicModifyIORef' time (\now -> (max now target, ()))
    pure (pure ())

main :: IO ()
main = do
  args <- getArgs
  (mode, duration) <- case args of
    [] -> pure ("sim", 3600)
    [mode] | validMode mode -> pure (mode, if mode == "sim" then 3600 else 60)
    [mode, seconds] | validMode mode -> case readMaybe seconds :: Maybe Integer of
      Just n | n >= 0 -> pure (mode, fromInteger n)
      _ -> die usage
    _ -> die usage
  clock <- if mode == "live" then newLiveClock else terminalSimulationClock
  start <- clockNow clock
  let initial = newEngine start duration roster
  runtime <- newLiveRuntime clock (const (pure ())) initial
  -- Seed the passive market maker before accepting terminal input.
  void $ requestLive runtime (PlayerId 2) (SubmitOrder (LimitOrder Buy (Price 9) 10))
  void $ requestLive runtime (PlayerId 2) (SubmitOrder (LimitOrder Sell (Price 11) 10))
  putStrLn $ "Trading game terminal (" ++ mode ++ ", " ++ show duration ++ ")."
  putStrLn "Players: alice, market-maker. Start with: join alice"
  putStrLn commandHelp
  if mode == "live"
    then runIO $ runTerminal $ runConcurrent $
      withWorkers [liftIO (void (runExchange runtime))] $
        runPlayerSession (players initial) (terminalSession runtime)
    else runIO $ runTerminal $
      runPlayerSession (players initial) (terminalSession runtime)
  where
    validMode mode = mode == "sim" || mode == "live"
    usage = "Usage: trading-game-terminal [sim|live] [SECONDS (nonnegative integer)]"
