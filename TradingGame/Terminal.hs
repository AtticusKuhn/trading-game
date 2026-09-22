{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module TradingGame.Terminal
  ( runTerminal, terminalSession, parseCommand, renderInfo, commandHelp ) where

import Control.Effect (Eff, IOE, (:<), interpret, liftIO)
import Control.Monad (void)
import Data.Char (isDigit, isSpace)
import Data.Ratio ((%), numerator, denominator)
import System.IO (hFlush, isEOF, stdout)
import Text.Read (readMaybe)
import TradingGame.Core
import TradingGame.Interaction
import TradingGame.Live (LiveRuntime, runWithCurrentPlayer)
import TradingGame.Session

-- The join loop has only session/interaction effects. TradingGame is installed
-- inside a successful login, so logged-out input cannot submit orders.
terminalSession
  :: (PlayerSession :< effs, PlayerInteraction :< effs, IOE :< effs)
  => LiveRuntime -> Eff effs ()
terminalSession runtime = joinLoop
  where
    joinLoop = do
      line <- liftIO $ do
        putStr "session> "
        hFlush stdout
        eof <- isEOF
        if eof then pure "quit" else getLine
      case words line of
        ["quit"] -> pure ()
        ["help"] -> liftIO (putStrLn "Join with: join NAME. Exit with: quit.") >> joinLoop
        "join":_:_ -> do
          -- Preserve spaces within a name; the separator after 'join' is syntax.
          let name = dropWhile isSpace (drop 4 (dropWhile isSpace line))
          result <- joinGameAsPlayer name
          case result of
            Left problem -> liftIO (print problem) >> joinLoop
            Right _ -> do
              liftIO (putStrLn ("Joined as " ++ name ++ "."))
              outcome <- runWithCurrentPlayer runtime gameLoop
              void logout
              case outcome of
                Left problem -> liftIO (print problem) >> joinLoop
                Right LeaveGame -> joinLoop
                Right _ -> pure ()
        _ -> liftIO (putStrLn "Join first with: join NAME (or quit).") >> joinLoop
    gameLoop = do
      command <- readInput
      case command of
        Quit -> pure Quit
        LeaveGame -> pure LeaveGame
        other -> execute other >> gameLoop

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
  , "          private | book | wait SECONDS | settlement | help | logout | quit"
  , "Prices and seconds accept integers, decimals, or fractions (e.g. 3/2)."
  , "settlement waits for closure; logout returns to joining; quit (or EOF) exits."
  ]

parseCommand :: String -> Either String PlayerCommand
parseCommand line = case words line of
  ["quit"] -> Right Quit
  ["logout"] -> Right LeaveGame
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
