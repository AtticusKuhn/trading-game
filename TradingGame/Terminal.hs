{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module TradingGame.Terminal (runTerminal, parseCommand, renderInfo, commandHelp) where

import Control.Effect (Eff, IOE, (:<), interpret, liftIO)
import Data.Char (isDigit)
import Data.Ratio ((%), numerator, denominator)
import System.IO (hFlush, isEOF, stdout)
import Text.Read (readMaybe)
import TradingGame.Core
import TradingGame.Interaction

runTerminal :: IOE :< effs => Eff (PlayerInteraction ': effs) a -> Eff effs a
runTerminal = interpret $ \request -> case request of
  ReadInput -> liftIO prompt
  SendInfo info -> liftIO (putStrLn (renderInfo info))
  where
    prompt = do
      putStr "trade> "
      hFlush stdout
      eof <- isEOF
      if eof then pure Quit else do
        line <- getLine
        case parseCommand line of
          Right command -> pure command
          Left message -> putStrLn message >> prompt

commandHelp :: String
commandHelp = unlines
  [ "Commands: buy PRICE QUANTITY | sell PRICE QUANTITY"
  , "          private | book | wait SECONDS | settlement | help | quit"
  , "Prices and seconds accept integers, decimals, or fractions (e.g. 3/2)."
  , "settlement waits for closure; quit (or EOF) ends this player's program."
  ]

parseCommand :: String -> Either String PlayerCommand
parseCommand line = case words line of
  ["quit"] -> Right Quit
  ["private"] -> Right ShowPrivateNumber
  ["book"] -> Right ShowExchange
  ["settlement"] -> Right ShowSettlement
  ["help"] -> Right Help
  ["wait", seconds] -> case parseNumber seconds of
    Just n | n >= 0 -> Right (WaitFor (fromRational n))
    _ -> Left "Expected a nonnegative number of seconds."
  [side, price, quantity] | side == "buy" || side == "sell" ->
    case (parseNumber price, readMaybe quantity) of
      (Just p, Just q) | q > 0 -> Right $ PlaceOrder $
        LimitOrder (if side == "buy" then Buy else Sell) (Price p) q
      _ -> Left "Expected a numeric price and a positive integer quantity."
  _ -> Left ("Unrecognized command. " ++ commandHelp)

-- Parse exact prices without introducing floating-point rounding or accepting
-- NaN/Infinity. A zero denominator is rejected before constructing a Ratio.
parseNumber :: String -> Maybe Rational
parseNumber input = case input of
  '-':rest -> negate <$> unsigned rest
  '+':rest -> unsigned rest
  _ -> unsigned input
  where
    digits s | not (null s) && all isDigit s = readMaybe s
             | otherwise = Nothing
    unsigned s = case break (`elem` "/.") s of
      (whole, []) -> fromInteger <$> digits whole
      (whole, '/':rest) -> do
        n <- digits whole
        d <- digits rest
        if d == 0 then Nothing else Just (n % d)
      (whole, '.':rest) -> do
        n <- digits whole
        fraction <- digits rest
        pure (fromInteger n + fraction % (10 ^ length rest))
      _ -> Nothing

renderInfo :: PlayerInfo -> String
renderInfo info = case info of
  PrivateNumber value -> "Your private number: " ++ show value
  ExchangeSnapshot snapshot -> show snapshot
  OrderSubmitted (Right oid) -> "Order accepted: " ++ show oid
  OrderSubmitted (Left problem) -> "Order rejected: " ++ show problem
  PlayerSettlement result -> "Resolved sum: " ++ show (resolvedSum result)
    ++ "; your payoff: " ++ renderRational (netPayoff result)
  HelpInfo -> commandHelp
  where
    renderRational n | denominator n == 1 = show (numerator n)
                     | otherwise = show (numerator n) ++ "/" ++ show (denominator n)
