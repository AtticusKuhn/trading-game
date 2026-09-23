{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module TradingGame.Terminal
  ( runTerminal, terminalSession, terminalSessionWith, parseCommand, renderInfo, commandHelp ) where

import Control.Effect (Eff, IOE, (:<), interpret, liftIO)
import Control.Monad (void)
import Data.Char (isDigit, isSpace, toLower)
import Data.List (find, intercalate)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Ratio ((%))
import System.IO (hFlush, isEOF, stdout)
import Text.Read (readMaybe)
import TradingGame.Concurrent
import TradingGame.Core
import TradingGame.Display (renderHolding, renderNumber)
import TradingGame.Interaction
import TradingGame.Live (LiveRuntime, runWithCurrentPlayer)
import TradingGame.Session

-- The join loop has only session/interaction effects. TradingGame is installed
-- inside a successful login, so logged-out input cannot submit orders.
terminalSession
  :: (PlayerSession :< effs, PlayerInteraction :< effs, IOE :< effs)
  => LiveRuntime -> Eff effs ()
terminalSession runtime = terminalSessionWith (runConcurrent . runWithCurrentPlayer runtime)

terminalSessionWith
  :: (PlayerSession :< effs, PlayerInteraction :< effs, IOE :< effs)
  => (Eff (TradingGame ': Concurrent ': effs) InteractionExit -> Eff effs (Either SessionError InteractionExit))
  -> Eff effs ()
terminalSessionWith runPlayer = joinLoop
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
              outcome <- runPlayer interactivePlayer
              void logout
              case outcome of
                Left problem -> liftIO (print problem) >> joinLoop
                Right LeftGame -> joinLoop
                Right _ -> pure ()
        _ -> liftIO (putStrLn "Join first with: join NAME (or quit).") >> joinLoop

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
  [ "Commands: buy INSTRUMENT PRICE QUANTITY | sell INSTRUMENT PRICE QUANTITY"
  , "          private | book | wait SECONDS | settlement | help | logout | quit"
  , "Instruments: Sum Range Min Max Median StdDev (omitting it defaults to Sum)."
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
    parseCommand (unwords [side, "Sum", price, quantity])
  [side, asset, price, quantity] | side == "buy" || side == "sell" ->
    case (find ((== map toLower asset) . map toLower . show) [minBound .. maxBound], parseNumber price, readMaybe quantity) of
      (Just selected, Just p, Just q) | q > 0 -> Right $ PlaceOrder $
        LimitOrder (if side == "buy" then Buy else Sell) (Price p) selected q
      _ -> Left "Expected a known instrument, numeric price, and positive integer quantity."
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
  ExchangeSnapshot snapshot ->
    let assets = Set.toAscList (enabledInstruments (gameInfo snapshot))
        row = intercalate " | "
    in unlines $
      [ "Exchange: " ++ show (gamePhase snapshot) ++ "; observed at: " ++ show (observedAt snapshot)
      , "Game: " ++ show (gameInfo snapshot)
      , "Books: " ++ show (orderBook snapshot)
      , "Trades: " ++ show (tradeHistory snapshot)
      , "Revealed numbers: " ++ show (revealedNumbers snapshot)
      , "Public portfolios (filled trades only):"
      , row (["Player"] ++ map show assets ++ ["Cash"])
      ] ++ [row ([portfolioName portfolio]
            ++ map (renderHolding portfolio) assets
            ++ [renderRational (portfolioCash portfolio)])
           | portfolio <- Map.elems (portfolios snapshot)]
  OrderSubmitted (Right oid) -> "Order accepted: " ++ show oid
  OrderSubmitted (Left problem) -> "Order rejected: " ++ show problem
  PlayerSettlement result -> "Resolutions: " ++ unwords [show asset ++ "=" ++ renderRational value
      | (asset, value) <- Map.toAscList (resolutions result)]
    ++ "; your payoff: " ++ renderRational (netPayoff result)
    ++ concatMap (\entry -> "\n" ++ displayName (settledPlayer entry)
      ++ "; private number: " ++ show (privateNumber (settledPlayer entry))
      ++ "; payoff: " ++ renderRational (playerPayoff entry)) (playerResults result)
  HelpInfo -> commandHelp
  where
    renderRational = renderNumber
