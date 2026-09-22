{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

-- A small in-process exchange for trusted player programs.
module TradingGame.Core where

import Control.Effect (Eff, Effect, (:<), send)
import Data.Char (isSpace)
import Data.List (find, nub, sortOn)
import Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime)

newtype PlayerId = PlayerId Integer deriving (Eq, Ord, Show)

-- Host-only identity: never include the roster in public exchange snapshots.
data Player = Player
  { playerID :: PlayerId
  , displayName :: String
  , privateNumber :: Integer
  } deriving (Eq, Show)

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

-- Only the trusted host receives this setup. Require N >= 1, unique IDs and unique nonblank names.
-- Each p_i is fixed at the start. The game closes exactly 3600 seconds later.
-- Never expose this setup to a player computation.
data GameSetup = GameSetup
  { setupPlayers :: [Player]
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
  } deriving (Eq, Show)

-- All trading rules operate on this host-only state. Times supplied to the
-- engine must be nondecreasing and no earlier than opensAt.
data Engine = Engine
  { engineInfo :: GameInfo
  , players :: [Player]
  , enginePhase :: GamePhase
  , engineBook :: !Exchange
  } deriving (Eq, Show)

-- The engine describes scheduling without performing it. A settlement is only
-- calculated once resolved, using every fill accepted before the deadline.
data Decision a where
  Reply :: a -> Decision a
  ResumeAt :: UTCTime -> Decision ()
  WhenResolved :: Decision Settlement

deriving instance Eq a => Eq (Decision a)
deriving instance Show a => Show (Decision a)

-- Names are exact, case-sensitive identities. Reject ambiguous rosters up front.
-- Nonpositive durations produce a game that is closed at its start.
newEngine :: UTCTime -> NominalDiffTime -> [Player] -> Engine
newEngine start duration roster
  | null roster = error "newEngine: empty player roster"
  | length (nub (map playerID roster)) /= length roster =
      error "newEngine: duplicate player IDs"
  | length (nub (map displayName roster)) /= length roster =
      error "newEngine: duplicate display names"
  | any (all isSpace . displayName) roster = error "newEngine: empty display name"
  | otherwise = Engine
      { engineInfo = GameInfo (length roster) start (addUTCTime (max 0 duration) start)
      , players = roster
      , enginePhase = Trading
      , engineBook = Exchange 1 [] [] [(playerID player, (0, 0)) | player <- roster]
      }

engineTotal :: Engine -> Integer
engineTotal = sum . map privateNumber . players

engineSettlements :: Engine -> [Settlement]
engineSettlements engine =
  [settlementFor (engineTotal engine) (playerID player) (engineBook engine)
  | player <- players engine]

advanceTo :: UTCTime -> Engine -> Engine
advanceTo now engine = case enginePhase engine of
  Trading | now >= closesAt (engineInfo engine) -> engine
    { enginePhase = Resolved (engineTotal engine)
    , engineBook = (engineBook engine) { ownedOrders = [] }
    }
  _ -> engine

-- Caller identity is supplied by the host, never by the player effect.
-- Deadline checking precedes every request, including quantity validation.
handleRequest
  :: UTCTime -> PlayerId -> TradingGame m a -> Engine -> (Engine, Decision a)
handleRequest now pid request initial =
  let engine = advanceTo now initial
      state = engineBook engine
      answer :: b -> (Engine, Decision b)
      answer value = (engine, Reply value)
  in case request of
    GetMyPrivateNumber -> answer $ case find ((== pid) . playerID) (players engine) of
      Just player -> privateNumber player
      Nothing -> error "handleRequest: unknown player ID"
    GetExchangeState -> answer ExchangeState
      { gameInfo = engineInfo engine
      , observedAt = now
      , gamePhase = enginePhase engine
      , orderBook = map snd (ownedOrders state)
      , tradeHistory = reverse (executedTrades state)
      }
    SubmitOrder order -> case enginePhase engine of
      Resolved _ -> answer (Left GameClosed)
      Trading
        | orderQuantity order <= 0 -> answer (Left InvalidQuantity)
        | otherwise ->
            let oid = OrderId (nextOrderId state)
                updated = matchOrder now pid oid order
                  state { nextOrderId = nextOrderId state + 1 }
            in (engine { engineBook = updated }, Reply (Right oid))
    Wait seconds -> (engine, ResumeAt (addUTCTime (max 0 seconds) now))
    WaitUntil target -> (engine, ResumeAt (max now target))
    AwaitSettlement -> case enginePhase engine of
      Resolved value -> answer (settlementFor value pid state)
      Trading -> (engine, WhenResolved)

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
