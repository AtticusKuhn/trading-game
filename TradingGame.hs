{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

-- A small in-process exchange for trusted player programs.
module TradingGame where

import Control.Effect (Eff, Effect, (:<), control0, handle, run, send)
import Data.List (sortOn)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), NominalDiffTime, addUTCTime)

newtype PlayerId = PlayerId Integer deriving (Eq, Ord, Show)
newtype OrderId = OrderId Integer deriving (Eq, Ord, Show)
newtype Price = Price Rational deriving (Eq, Ord, Show)

-- Assumption: limit orders for positive quantities of a single contract on S.
-- A buy specifies the maximum price; a sell specifies the minimum price.
-- Price units are the same as S; prices may be negative if S can be negative.
data LimitOrder = LimitOrder
  { orderSide :: Side
  , limitPrice :: Price
  , orderQuantity :: Integer
  } deriving (Eq, Show)

data Side = Buy | Sell deriving (Eq, Show)

-- Only the trusted host receives this setup. Require N >= 1 and unique IDs.
-- Each p_i is fixed at the start. The game closes exactly 3600 seconds later.
-- Never expose this setup to a player computation.
data GameSetup = GameSetup
  { privateNumbers :: [(PlayerId, Integer)]
  , startTime :: UTCTime
  } deriving (Eq)

data GameInfo = GameInfo
  { playerCount :: Int
  , opensAt :: UTCTime
  , closesAt :: UTCTime
  } deriving (Eq, Show)

-- The sum is public only once the game resolves; individual numbers remain
-- private even after resolution. Outstanding orders expire at the deadline.
data GamePhase = Trading | Resolved Integer deriving (Eq, Show)

-- Public order book entries contain the unfilled quantity, not private numbers.
data RestingOrder = RestingOrder
  { restingOrderId :: OrderId
  , restingSide :: Side
  , restingPrice :: Price
  , remainingQuantity :: Integer
  } deriving (Eq, Show)

data Trade = Trade
  { tradePrice :: Price
  , tradeQuantity :: Integer
  , tradedAt :: UTCTime
  } deriving (Eq, Show)

-- One consistent public snapshot, available throughout trading and afterwards.
-- The book contains all outstanding buys and sells; trades are executed fills.
data ExchangeState = ExchangeState
  { gameInfo :: GameInfo
  , observedAt :: UTCTime
  , gamePhase :: GamePhase
  , orderBook :: [RestingOrder]
  , tradeHistory :: [Trade]
  } deriving (Eq, Show)

data OrderError = GameClosed | InvalidQuantity deriving (Eq, Show)

-- Acceptance does not imply execution: an order may fill partially or rest.
-- The interpreter below uses price/time priority and the resting order price.
-- It allows unlimited positions and self-trades.
type OrderResult = Either OrderError OrderId

-- One filled unit bought at price P pays S - P; a sale pays P - S.
-- Unfilled orders have no payoff. This result belongs only to the caller.
data Settlement = Settlement
  { resolvedSum :: Integer
  , netPayoff :: Rational
  } deriving (Eq, Show)

-- The host binds this effect to an authenticated player before running it.
-- Requests cannot select another player or access the host's private setup.
-- All player interpreters must share the same exchange and authoritative clock.
-- At/after closesAt, submissions fail and snapshots show Resolved (sum p_i),
-- even if no player has made a request since the deadline.
data TradingGame :: Effect where
  GetMyPrivateNumber :: TradingGame m Integer
  GetExchangeState :: TradingGame m ExchangeState
  SubmitOrder :: LimitOrder -> TradingGame m OrderResult
  AwaitSettlement :: TradingGame m Settlement
  Wait :: NominalDiffTime -> TradingGame m ()
  WaitUntil :: UTCTime -> TradingGame m ()

-- Request helpers follow the NumberGame/send style in guess_the_number.hs.
getMyPrivateNumber :: TradingGame :< effs => Eff effs Integer
getMyPrivateNumber = send GetMyPrivateNumber

getExchangeState :: TradingGame :< effs => Eff effs ExchangeState
getExchangeState = send GetExchangeState

submitOrder :: TradingGame :< effs => LimitOrder -> Eff effs OrderResult
submitOrder = send . SubmitOrder

-- Wait until the deadline, or return immediately if already resolved.
awaitSettlement :: TradingGame :< effs => Eff effs Settlement
awaitSettlement = send AwaitSettlement

-- Sleep in game time. Nonpositive durations and past targets do not advance it.
-- Resting orders can still fill while the player sleeps.
wait :: TradingGame :< effs => NominalDiffTime -> Eff effs ()
wait = send . Wait

waitUntil :: TradingGame :< effs => UTCTime -> Eff effs ()
waitUntil = send . WaitUntil


-- Host-only state. The list order breaks ties between orders at the same price.
-- Accounts hold (net units bought, cash received); shorts are negative units.
data Exchange = Exchange
  { nextOrderId :: Integer
  , ownedOrders :: [(PlayerId, RestingOrder)]
  , executedTrades :: [Trade]
  , accounts :: [(PlayerId, (Integer, Rational))]
  }

-- A suspended request retains its typed continuation, including any local
-- variables in the player's program. No OS threads or wall-clock sleeps are used.
data PlayerStep where
  Finished :: PlayerStep
  Requested :: TradingGame m a -> (a -> Eff '[TradingGame] ()) -> PlayerStep

stepPlayer :: Eff '[TradingGame] () -> PlayerStep
stepPlayer = run . handle (\() -> pure Finished)
  (\request -> control0 $ \resume -> pure (Requested request resume))

data Event
  = CloseExchange
  | ResumePlayer PlayerId Int (Eff '[TradingGame] ())

-- Fixed epoch makes repeated simulations identical. Players can read opensAt
-- from GetExchangeState to construct absolute WaitUntil targets.
simulationStart :: UTCTime
simulationStart = UTCTime (fromGregorian 2000 1 1) 0

-- Supply unique player IDs; each Int is that player's private number.
-- Settlements are returned in the same order as the input players.
-- One hour of virtual time, plus any post-settlement player activity, runs as
-- fast as computation permits. The simulation is pure.
runTradingGame :: [(PlayerId, Eff '[TradingGame] (), Int)] -> [Settlement]
runTradingGame = runTradingGameFor 3600

runTradingGameFor
  :: NominalDiffTime -> [(PlayerId, Eff '[TradingGame] (), Int)] -> [Settlement]
runTradingGameFor = runTradingGameAt simulationStart

-- Explicit virtual epoch and duration for tests or dated scenarios.
runTradingGameAt'
  :: UTCTime -> NominalDiffTime -> [(PlayerId, Eff '[TradingGame] (), Int)] -> Exchange
runTradingGameAt' start duration players =
  let info = GameInfo (length players) start (addUTCTime duration start)
      total = sum [toInteger secret | (_, _, secret) <- players]
      initial = Exchange 1 [] [] [(pid, (0, 0)) | (pid, _, _) <- players]
      -- Stable sorting keeps closure ahead of every wake-up at the deadline.
      events = (closesAt info, CloseExchange) :
        [(start, ResumePlayer pid secret program)
        | (pid, program, secret) <- players]
  in simulate info total Trading initial (sortOn fst events)


runTradingGameAt
  :: UTCTime -> NominalDiffTime -> [(PlayerId, Eff '[TradingGame] (), Int)] -> [Settlement]
runTradingGameAt start duration players =
  let final = runTradingGameAt' start duration players
      total = sum [toInteger secret | (_, _, secret) <- players]
  in [settlementFor total pid final | (pid, _, _) <- players]

-- Each request yields to other runnable players at the same virtual time.
-- When none remain, the next event jumps the clock directly to its timestamp.
-- Infinite immediate requests (including Wait 0) prevent time from advancing;
-- polling strategies should use a positive Wait. Pure nontermination also blocks.
-- Waits beyond the deadline are honored; the exchange stays resolved thereafter.
simulate :: GameInfo -> Integer -> GamePhase -> Exchange
  -> [(UTCTime, Event)] -> Exchange
simulate _ _ _ state [] = state
simulate info total phase state ((now, event) : pending) = case event of
  CloseExchange ->
    simulate info total (Resolved total) state { ownedOrders = [] } pending
  ResumePlayer pid secret program -> case stepPlayer program of
    Finished -> simulate info total phase state pending
    Requested request resume ->
      let continueAt wake result updated =
            simulate info total phase updated $ sortOn fst
              (pending ++ [(max now wake, ResumePlayer pid secret (resume result))])
          continue = continueAt now
      in case request of
        GetMyPrivateNumber -> continue (toInteger secret) state
        GetExchangeState -> continue ExchangeState
          { gameInfo = info
          , observedAt = now
          , gamePhase = phase
          , orderBook = map snd (ownedOrders state)
          , tradeHistory = reverse (executedTrades state)
          } state
        SubmitOrder order -> case phase of
          Resolved _ -> continue (Left GameClosed) state
          Trading
            | orderQuantity order <= 0 -> continue (Left InvalidQuantity) state
            | otherwise ->
                let oid = OrderId (nextOrderId state)
                    updated = matchOrder now pid oid order
                      state { nextOrderId = nextOrderId state + 1 }
                in continue (Right oid) updated
        Wait seconds -> continueAt (addUTCTime (max 0 seconds) now) () state
        WaitUntil target -> continueAt target () state
        AwaitSettlement -> case phase of
          Resolved value -> continue (settlementFor value pid state) state
          Trading ->
            -- Reissue at closure so the payoff uses all fills before settlement.
            simulate info total phase state $ sortOn fst
              (pending ++ [(closesAt info, ResumePlayer pid secret
                (awaitSettlement >>= resume))])

settlementFor :: Integer -> PlayerId -> Exchange -> Settlement
settlementFor total pid state =
  let (units, cash) = maybe (0, 0) id (lookup pid (accounts state))
  in Settlement total (cash + fromInteger (units * total))

-- Match against the best eligible price, then the oldest order at that price.
-- An incoming order's remainder rests until matched or the game closes.
matchOrder :: UTCTime -> PlayerId -> OrderId -> LimitOrder -> Exchange -> Exchange
matchOrder now pid oid order state
  | orderQuantity order == 0 = state
  | otherwise = case sortOn priority eligible of
      [] -> state { ownedOrders = ownedOrders state ++ [(pid, resting)] }
      (owner, maker) : _ ->
        let quantity = min (orderQuantity order) (remainingQuantity maker)
            Price price = restingPrice maker
            signed = if orderSide order == Buy then quantity else negate quantity
            updateAccount (who, (units, cash))
              | pid == owner = (who, (units, cash))
              | who == pid = (who, (units + signed, cash - fromInteger signed * price))
              | who == owner = (who, (units - signed, cash + fromInteger signed * price))
              | otherwise = (who, (units, cash))
            updateResting (who, entry)
              | restingOrderId entry /= restingOrderId maker = [(who, entry)]
              | remainingQuantity entry == quantity = []
              | otherwise = [(who, entry
                  { remainingQuantity = remainingQuantity entry - quantity })]
            updated = state
              { ownedOrders = concatMap updateResting (ownedOrders state)
              , executedTrades = Trade (restingPrice maker) quantity now : executedTrades state
              , accounts = map updateAccount (accounts state)
              }
        in matchOrder now pid oid
             order { orderQuantity = orderQuantity order - quantity } updated
  where
    resting = RestingOrder oid (orderSide order) (limitPrice order) (orderQuantity order)
    eligible = filter crosses (ownedOrders state)
    crosses (_, maker) = restingSide maker /= orderSide order
      && case orderSide order of
        Buy -> restingPrice maker <= limitPrice order
        Sell -> restingPrice maker >= limitPrice order
    priority (_, maker) =
      let Price price = restingPrice maker
      in (if orderSide order == Buy then price else negate price, restingOrderId maker)
