{-# LANGUAGE DataKinds #-}

module Main where

import Control.Effect (liftIO, runIO)
import qualified Control.Effect.State.Strict as State
import Control.Monad (void)
import System.Environment (getArgs)
import System.Exit (die)
import Text.Read (readMaybe)
import TradingGame
import TradingGame.Terminal

-- The host fixes the roster before anyone joins. Joining never allocates a
-- private number or account. These names are selectors, not authentication.
roster :: [Player]
roster = [Player (PlayerId 1) "alice" 3, Player (PlayerId 2) "market-maker" 7]

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
  let initial start = foldl (\engine order -> fst (handleRequest start (PlayerId 2) (SubmitOrder order) engine))
        (newEngine start duration roster)
        [LimitOrder Buy (Price 9) 10, LimitOrder Sell (Price 11) 10]
  putStrLn $ "Trading game terminal (" ++ mode ++ ", " ++ show duration ++ ")."
  putStrLn "Players: alice, market-maker. Start with: join alice"
  putStrLn commandHelp
  if mode == "live" then do
    clock <- newLiveClock
    start <- clockNow clock
    runtime <- newLiveRuntime clock (const (pure ())) (initial start)
    runIO $ runTerminal $ runConcurrent $
      withWorkers [liftIO (void (runExchange runtime))] $
        runPlayerSession roster (terminalSession runtime)
  else runIO $ runTerminal $ runPlayerSession roster $
    State.evalState (simulationStart, initial simulationStart) $ terminalSessionWith $ \program -> do
      selected <- getCurrentPlayer
      case selected of
        Nothing -> pure (Left NotLoggedIn)
        Just pid -> do
          (now, engine) <- State.get
          (stoppedAt, updated, outcome) <- runSimulatedPlayer now engine pid program
          State.put (stoppedAt, updated)
          pure (Right outcome)
  where
    validMode mode = mode == "sim" || mode == "live"
    usage = "Usage: trading-game-terminal [sim|live] [SECONDS (nonnegative integer)]"
