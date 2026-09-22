module Main where

import System.Environment (getArgs)
import System.Exit (die)
import Text.Read (readMaybe)
import TradingGame.Web (runWebServer)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runWebServer 3000 3600
    [port] -> start port "3600"
    [port, seconds] -> start port seconds
    _ -> die usage
  where
    start port seconds = case (readMaybe port, readMaybe seconds :: Maybe Integer) of
      (Just p, Just duration) | p > 0 && p <= 65535 && duration > 0 && duration <= 86400 ->
        runWebServer p (fromInteger duration)
      _ -> die usage
    usage = "Usage: trading-game-web [PORT (1–65535)] [SUGGESTED_SECONDS (1–86400, default 3600)]"
