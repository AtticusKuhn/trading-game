{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Effect ((:<), runIO)
import Control.Monad (void)
import System.Environment (getArgs)
import System.Exit (die)
import Text.Read (readMaybe)
import TradingGame
import TradingGame.Terminal

-- One human and a small passive market maker. Setup belongs to the host; the
-- interaction handler only receives public snapshots and the human's replies.
players :: PlayerInteraction :< effs => [Player effs]
players =
  [ (PlayerId 2, do
      void (submitOrder (LimitOrder Buy (Price 9) 10))
      void (submitOrder (LimitOrder Sell (Price 11) 10)), 7)
  , (PlayerId 1, interactivePlayer, 3)
  ]

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
  putStrLn $ "Trading game terminal (" ++ mode ++ ", " ++ show duration ++ ")."
  putStrLn commandHelp
  if mode == "live"
    then putStrLn "The live session ends at closure, even if you quit early."
    else putStrLn "Virtual time advances when players wait or finish."
  settlements <- case mode of
    "live" -> runIO $ runTerminal $ runConcurrent $ runLiveFor duration players
    _ -> runIO $ runTerminal $ runTradingGameFor duration players
  -- Settlements follow setup order; only show the human's own payoff.
  case drop 1 settlements of
    result:_ -> putStrLn (renderInfo (PlayerSettlement result))
    [] -> pure ()
  where
    validMode mode = mode == "sim" || mode == "live"
    usage = "Usage: trading-game-terminal [sim|live] [SECONDS (nonnegative integer)]"

