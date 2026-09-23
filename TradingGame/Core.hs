{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

-- A small in-process exchange for trusted player programs.
module TradingGame.Core where

import Control.Effect (Eff, Effect, (:<), send)
import Data.Char (isSpace)
import Data.List (find, nub, sort, sortOn)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Ratio ((%), numerator, denominator)
import Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime, diffUTCTime)

newtype PlayerId = PlayerId Integer deriving (Eq, Ord, Show)

-- Identities stay private during trading; scheduled events reveal only numbers.
data Player = Player
  { playerID :: PlayerId
  , displayName :: String
  , privateNumber :: Integer
  } deriving (Eq, Show)

newtype OrderId = OrderId Integer deriving (Eq, Ord, Show)
newtype Price = Price Rational deriving (Eq, Ord, Show)

data Instrument = Sum | Range | Min | Max | Median | StdDev
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

allInstruments :: Set Instrument
allInstruments = Set.fromList [minBound .. maxBound]

type Positions = Map Instrument Integer
type Resolutions = Map Instrument Rational

data Account = Account
  { cash :: Rational
  , positions :: Positions
  } deriving (Eq, Show)

-- The host guarantees a nonempty roster. Median averages the middle pair.
-- Population standard deviation rounds to the nearest millionth (ties up).
-- Integer arithmetic avoids overflow and cancellation for large private numbers.
resolve :: Instrument -> [Integer] -> Rational
resolve _ [] = error "resolve: empty player roster"
resolve asset values = case asset of
  Sum -> fromInteger (sum values)
  Range -> fromInteger (maximum values - minimum values)
  Min -> fromInteger (minimum values)
  Max -> fromInteger (maximum values)
  Median -> let ordered = sort values
                middle = length values `div` 2
            in if odd (length values) then fromInteger (ordered !! middle)
               else (ordered !! (middle - 1) + ordered !! middle) % 2
  StdDev -> roundedSquareRoot variance
  where
    count = toInteger (length values)
    mean = sum values % count
    variance = sum [(fromInteger value - mean) ^ (2 :: Int) | value <- values] / fromInteger count

roundedSquareRoot :: Rational -> Rational
roundedSquareRoot value =
  let scale = 1000000
      scaled = value * fromInteger (scale * scale)
      lower = integerSquareRoot (numerator scaled `div` denominator scaled)
      rounded = if scaled >= (2 * lower + 1) ^ (2 :: Int) % 4 then lower + 1 else lower
  in rounded % scale

integerSquareRoot :: Integer -> Integer
integerSquareRoot n
  | n < 0 = error "integerSquareRoot: negative input"
  | n == 0 = 0
  | otherwise = descend n
  where
    descend x = let next = (x + n `div` x) `div` 2
                in if next >= x then x else descend next

-- A buy specifies the maximum price; a sell specifies the minimum price.
data LimitOrder = LimitOrder
  { orderSide :: Side
  , limitPrice :: Price
  , instrument :: Instrument
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
  , enabledInstruments :: Set Instrument
  , revealTimes :: [UTCTime]
  } deriving (Eq, Show)

-- Resolutions and the full player-to-number mapping appear at settlement.
-- Outstanding orders expire at the deadline.
data GamePhase = Trading | Resolved Resolutions deriving (Eq, Show)

-- Public order book entries contain the unfilled quantity, not private numbers.
data RestingOrder = RestingOrder
  { restingOrderId :: OrderId
  , restingSide :: Side
  , restingPrice :: Price
  , remainingQuantity :: Integer
  } deriving (Eq, Show)

data Trade = Trade
  { tradeInstrument :: Instrument
  , tradePrice :: Price
  , tradeQuantity :: Integer
  , tradedAt :: UTCTime
  } deriving (Eq, Show)

-- One consistent public snapshot, available throughout trading and afterwards.
-- The book contains all outstanding buys and sells; trades are executed fills.
data ExchangeState = ExchangeState
  { gameInfo :: GameInfo
  , observedAt :: UTCTime
  , gamePhase :: GamePhase
  , orderBook :: Map Instrument [RestingOrder]
  , tradeHistory :: [Trade]
  , revealedNumbers :: [Integer]
  } deriving (Eq, Show)

data OrderError = GameClosed | InvalidQuantity | InstrumentDisabled deriving (Eq, Show)

-- Acceptance does not imply execution: an order may fill partially or rest.
-- The interpreter below uses price/time priority and the resting order price.
-- It allows unlimited positions and self-trades.
type OrderResult = Either OrderError OrderId

-- One bought unit pays its resolution minus its execution price; a sale pays
-- the opposite.
-- Unfilled orders have no payoff. Every caller sees the complete final roster.
data PlayerResult = PlayerResult
  { settledPlayer :: Player
  , playerPayoff :: Rational
  } deriving (Eq, Show)

data Settlement = Settlement
  { resolutions :: Resolutions
  , netPayoff :: Rational
  , playerResults :: [PlayerResult]
  } deriving (Eq, Show)

data TradingGame :: Effect where
  GetMyPrivateNumber :: TradingGame m Integer
  GetExchangeState :: TradingGame m ExchangeState
  AwaitExchangeChange :: ExchangeState -> TradingGame m ExchangeState
  SubmitOrder :: LimitOrder -> TradingGame m OrderResult
  AwaitSettlement :: TradingGame m Settlement
  AwaitUntilNextReveal :: TradingGame m (Maybe Integer)
  Wait :: NominalDiffTime -> TradingGame m ()
  WaitUntil :: UTCTime -> TradingGame m ()

-- Request helpers follow the NumberGame/send style in guess_the_number.hs.
getMyPrivateNumber :: TradingGame :< effs => Eff effs Integer
getMyPrivateNumber = send GetMyPrivateNumber

getExchangeState :: TradingGame :< effs => Eff effs ExchangeState
getExchangeState = send GetExchangeState

-- Wait for a public change since this snapshot. Passing the snapshot avoids
-- losing a change between rendering it and subscribing, including at closure.
awaitExchangeChange :: TradingGame :< effs => ExchangeState -> Eff effs ExchangeState
awaitExchangeChange = send . AwaitExchangeChange

submitOrder :: TradingGame :< effs => LimitOrder -> Eff effs OrderResult
submitOrder = send . SubmitOrder

-- Wait until the deadline, or return immediately if already resolved.
awaitSettlement :: TradingGame :< effs => Eff effs Settlement
awaitSettlement = send AwaitSettlement

-- Wait for the first strictly future reveal. Simultaneous events are all
-- published together; this returns the first one's number, not a backlog.
awaitUntilNextReveal :: TradingGame :< effs => Eff effs (Maybe Integer)
awaitUntilNextReveal = send AwaitUntilNextReveal

-- Sleep in game time. Nonpositive durations and past targets do not advance it.
-- Resting orders can still fill while the player sleeps.
wait :: TradingGame :< effs => NominalDiffTime -> Eff effs ()
wait = send . Wait

waitUntil :: TradingGame :< effs => UTCTime -> Eff effs ()
waitUntil = send . WaitUntil


-- Host-only state. The list order breaks ties between orders at the same price.
-- Accounts share cash across instruments; short positions are negative units.
type OrderBook = [(PlayerId, RestingOrder)]
data Exchange = Exchange
  { nextOrderId :: Integer
  , books :: Map Instrument OrderBook
  , executedTrades :: [Trade]
  , accounts :: Map PlayerId Account
  } deriving (Eq, Show)

-- All trading rules operate on this host-only state. Times supplied to the
-- engine must be nondecreasing and no earlier than opensAt.
data Engine = Engine
  { engineInfo :: GameInfo
  , players :: [Player]
  , enginePhase :: GamePhase
  , engineBook :: !Exchange
  , pendingReveals :: [(UTCTime, Integer)]
  , engineRevealedNumbers :: [Integer]
  } deriving (Eq, Show)

-- The engine describes scheduling without performing it. A settlement is only
-- calculated once resolved, using every fill accepted before the deadline.
data Decision a where
  Reply :: a -> Decision a
  ResumeAt :: UTCTime -> Decision ()
  WhenResolved :: Decision Settlement
  WhenExchangeChanges :: ExchangeState -> Decision ExchangeState
  WhenRevealed :: UTCTime -> Integer -> Decision (Maybe Integer)

deriving instance Eq a => Eq (Decision a)
deriving instance Show a => Show (Decision a)

-- Names are exact, case-sensitive identities. Reject ambiguous rosters up front.
-- Nonpositive durations produce a game that is closed at its start. These
-- low-level convenience constructors use an empty reveal plan; runners and
-- game creation supply sampled plans through newEngineWithReveals.
newEngine :: UTCTime -> NominalDiffTime -> [Player] -> Engine
newEngine = newEngineWithInstruments allInstruments

newEngineWithInstruments :: Set Instrument -> UTCTime -> NominalDiffTime -> [Player] -> Engine
newEngineWithInstruments enabled start duration roster =
  newEngineWithReveals enabled start duration roster []

-- Pure constructors never draw randomness. The host supplies a fixed plan of
-- roster identities, including repeated identities and simultaneous events.
newEngineWithReveals :: Set Instrument -> UTCTime -> NominalDiffTime -> [Player] -> [(UTCTime, PlayerId)] -> Engine
newEngineWithReveals enabled start duration roster plan
  | null roster = error "newEngine: empty player roster"
  | length (nub (map playerID roster)) /= length roster =
      error "newEngine: duplicate player IDs"
  | length (nub (map displayName roster)) /= length roster =
      error "newEngine: duplicate display names"
  | any (all isSpace . displayName) roster = error "newEngine: empty display name"
  | any (\(time, pid) -> time < start || time >= end || pid `notElem` map playerID roster) plan =
      error "newEngine: invalid reveal time or player ID"
  | otherwise = Engine
      { engineInfo = GameInfo (length roster) start end enabled (map fst ordered)
      , players = roster
      , enginePhase = Trading
      , engineBook = Exchange 1 (Map.fromSet (const []) enabled) []
          (Map.fromList [(playerID player, Account 0 Map.empty) | player <- roster])
      , pendingReveals = [(time, numberFor pid) | (time, pid) <- ordered]
      , engineRevealedNumbers = []
      }
  where
    end = addUTCTime (max 0 duration) start
    ordered = sortOn fst plan
    numberFor pid = case find ((== pid) . playerID) roster of
      Just player -> privateNumber player
      Nothing -> error "newEngine: unknown reveal target"

defaultRevealTimes :: UTCTime -> UTCTime -> Int -> [UTCTime]
defaultRevealTimes start end count
  | end <= start = []
  | otherwise = [addUTCTime (fromRational (toRational (diffUTCTime end start)
      * (toInteger i % (toInteger count + 1)))) start | i <- [1..count]]

engineResolutions :: Engine -> Resolutions
engineResolutions engine = Map.fromSet
  (\asset -> resolve asset (map privateNumber (players engine)))
  (enabledInstruments (engineInfo engine))

engineSettlements :: Engine -> [Settlement]
engineSettlements engine =
  [settlementFor (playerID player) engine
  | player <- players engine]

advanceTo :: UTCTime -> Engine -> Engine
advanceTo now initial = case enginePhase engine of
  Trading | now >= closesAt (engineInfo engine) -> engine
    { enginePhase = Resolved (engineResolutions engine)
    , engineBook = (engineBook engine) { books = Map.map (const []) (books (engineBook engine)) }
    }
  _ -> engine
  where
    (due, future) = span ((<= now) . fst) (pendingReveals initial)
    engine | null due = initial
           | otherwise = initial { pendingReveals = future
                                 , engineRevealedNumbers = engineRevealedNumbers initial ++ map snd due }

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
    GetExchangeState -> answer (exchangeSnapshot now engine)
    AwaitUntilNextReveal -> case pendingReveals engine of
      [] -> answer Nothing
      (time, value):_ -> (engine, WhenRevealed time value)
    AwaitExchangeChange previous
      | exchangeChanged previous (exchangeSnapshot now engine) -> answer (exchangeSnapshot now engine)
      | otherwise -> (engine, WhenExchangeChanges previous)
    SubmitOrder order -> case enginePhase engine of
      Resolved _ -> answer (Left GameClosed)
      Trading
        | instrument order `Set.notMember` enabledInstruments (engineInfo engine) -> answer (Left InstrumentDisabled)
        | orderQuantity order <= 0 -> answer (Left InvalidQuantity)
        | otherwise ->
            let oid = OrderId (nextOrderId state)
                updated = matchOrder now pid oid order
                  state { nextOrderId = nextOrderId state + 1 }
            in (engine { engineBook = updated }, Reply (Right oid))
    Wait seconds -> (engine, ResumeAt (addUTCTime (max 0 seconds) now))
    WaitUntil target -> (engine, ResumeAt (max now target))
    AwaitSettlement -> case enginePhase engine of
      Resolved _ -> answer (settlementFor pid engine)
      Trading -> (engine, WhenResolved)

-- Time passing alone is not a public exchange change.
exchangeChanged :: ExchangeState -> ExchangeState -> Bool
exchangeChanged before after =
  gamePhase before /= gamePhase after || orderBook before /= orderBook after
    || tradeHistory before /= tradeHistory after
    || revealedNumbers before /= revealedNumbers after

exchangeSnapshot :: UTCTime -> Engine -> ExchangeState
exchangeSnapshot now engine = ExchangeState
  { gameInfo = engineInfo engine
  , observedAt = now
  , gamePhase = enginePhase engine
  , orderBook = Map.map (map snd) (books (engineBook engine))
  , tradeHistory = reverse (executedTrades (engineBook engine))
  , revealedNumbers = engineRevealedNumbers engine
  }

-- Host-only helper; interpreters call this after resolution.
settlementFor :: PlayerId -> Engine -> Settlement
settlementFor pid engine = Settlement
  { resolutions = values
  , netPayoff = payoff pid
  , playerResults = [PlayerResult player (payoff (playerID player)) | player <- players engine]
  }
  where
    values = case enginePhase engine of
      Resolved resolved -> resolved
      Trading -> error "settlementFor: game has not resolved"
    payoff who =
      let account = Map.findWithDefault (Account 0 Map.empty) who (accounts (engineBook engine))
      in cash account + sum (Map.elems (Map.intersectionWith
           (\units value -> fromInteger units * value) (positions account) values))

-- Match against the best eligible price, then the oldest order at that price.
-- An incoming order's remainder rests until matched or the game closes.
matchOrder :: UTCTime -> PlayerId -> OrderId -> LimitOrder -> Exchange -> Exchange
matchOrder now pid oid order state
  | instrument order `Map.notMember` books state = state
  | orderQuantity order == 0 = state
  | otherwise = case sortOn priority eligible of
      [] -> state { books = Map.insert asset (book ++ [(pid, resting)]) (books state) }
      (owner, maker) : _ ->
        let quantity = min (orderQuantity order) (remainingQuantity maker)
            Price price = restingPrice maker
            signed = if orderSide order == Buy then quantity else negate quantity
            updateAccount who account
              | pid == owner = account
              | who == pid = adjust signed account
              | who == owner = adjust (negate signed) account
              | otherwise = account
            adjust units account = Account
              (cash account - fromInteger units * price)
              (Map.filter (/= 0) (Map.insertWith (+) asset units (positions account)))
            updateResting (who, entry)
              | restingOrderId entry /= restingOrderId maker = [(who, entry)]
              | remainingQuantity entry == quantity = []
              | otherwise = [(who, entry
                  { remainingQuantity = remainingQuantity entry - quantity })]
            updated = state
              { books = Map.insert asset (concatMap updateResting book) (books state)
              , executedTrades = Trade asset (restingPrice maker) quantity now : executedTrades state
              , accounts = Map.mapWithKey updateAccount (accounts state)
              }
        in matchOrder now pid oid
             order { orderQuantity = orderQuantity order - quantity } updated
  where
    asset = instrument order
    book = Map.findWithDefault [] asset (books state)
    resting = RestingOrder oid (orderSide order) (limitPrice order) (orderQuantity order)
    eligible = filter crosses book
    crosses (_, maker) = restingSide maker /= orderSide order
      && case orderSide order of
        Buy -> restingPrice maker <= limitPrice order
        Sell -> restingPrice maker >= limitPrice order
    priority (_, maker) =
      let Price price = restingPrice maker
      in (if orderSide order == Buy then price else negate price, restingOrderId maker)
