{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module EngineTests (engineProperties, discoverLaws) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (sort, transpose)
import Data.Ratio ((%))
import Data.Proxy (Proxy(..))
import Data.Time.Clock (addUTCTime)
import qualified QuickSpec as QS
import Test.QuickCheck
import TestSupport
import TradingGame

engineProperties :: [Property]
engineProperties =
  [ prop_conservation
  , prop_closure
  , prop_invalidOrder
  , prop_waits
  , prop_settlementDisclosure
  , prop_instrumentIsolation
  , prop_enabledInstruments
  , prop_portfolioPayoff
  , prop_publicPortfolios
  , prop_portfoliosTrackFills
  , prop_portfoliosKeepSecrets
  , prop_effectivePriceBreakEven
  , property prop_resolutionTranslation
  , property prop_populationStdDev
  , property prop_orderStatistics
  ]

prop_conservation :: Property
prop_conservation = forAll genEngine $ \engine ->
  let state = engineBook engine
      balances = Map.elems (accounts state)
      uncrossed book = and [restingPrice buy < restingPrice sell
                          | (_, buy) <- book, restingSide buy == Buy
                          , (_, sell) <- book, restingSide sell == Sell]
  in conjoin
    [ property (all (== 0) (Map.elems (Map.unionsWith (+) (map positions balances))))
    , sum (map cash balances) === 0
    , conjoin [sum (Map.elems (netSpent account)) === negate (cash account) | account <- balances]
    , property (all ((> 0) . remainingQuantity . snd) (concat (Map.elems (books state))))
    , property (all uncrossed (Map.elems (books state)))
    ]

-- Every observer sees the entire roster's accounts, before and after closure.
prop_publicPortfolios :: Property
prop_publicPortfolios = forAll genEngine $ \engine ->
  forAll (elements (players engine)) $ \viewer ->
    let snapshot = exchangeSnapshot simulationStart engine
        public = portfolios snapshot
        closed = advanceTo (closesAt (engineInfo engine)) engine
    in conjoin
      [ Map.map portfolioName public === Map.fromList [(playerID p, displayName p) | p <- players engine]
      , Map.map portfolioCash public === Map.map cash (accounts (engineBook engine))
      , Map.map portfolioPositions public === Map.map positions (accounts (engineBook engine))
      , conjoin
          [ conjoin
              [ Map.keysSet (portfolioEffectivePrices portfolio) === Map.keysSet (positions account)
              , conjoin [fromInteger (positions account Map.! asset) * price === netSpent account Map.! asset
                        | (asset, price) <- Map.toList (portfolioEffectivePrices portfolio)]
              ]
          | (pid, portfolio) <- Map.toList public, let account = accounts (engineBook engine) Map.! pid]
      , snd (handleRequest simulationStart (playerID viewer) GetExchangeState engine) === Reply snapshot
      , portfolios (exchangeSnapshot (closesAt (engineInfo engine)) closed) === public
      ]

-- Only the matched quantity transfers, even for partial fills and self-trades.
prop_portfoliosTrackFills :: Property
prop_portfoliosTrackFills = forAll genRoster $ \roster -> forAll genOrder $ \order ->
  forAll (elements roster) $ \maker -> forAll (elements roster) $ \taker ->
  forAll (getPositive <$> arbitrary) $ \quantity ->
    let initial = newEngine simulationStart 60 roster
        submit player value engine = fst (handleRequest simulationStart (playerID player) (SubmitOrder value) engine)
        resting = submit maker order initial
        opposite = case orderSide order of Buy -> Sell; Sell -> Buy
        filled = submit taker (order { orderSide = opposite, orderQuantity = quantity }) resting
        Price price = limitPrice order
        units = (if orderSide order == Buy then 1 else -1) * min quantity (orderQuantity order)
        expected player =
          let amount = (if player == maker then units else 0) - (if player == taker then units else 0)
          in PublicPortfolio (displayName player) (negate (fromInteger amount * price))
               (Map.filter (/= 0) (Map.singleton (instrument order) amount))
               (if amount == 0 then Map.empty else Map.singleton (instrument order) price)
        public = portfolios . exchangeSnapshot simulationStart
    in conjoin
      [ public initial === Map.fromList [(playerID p, PublicPortfolio (displayName p) 0 Map.empty Map.empty) | p <- roster]
      , public resting === public initial
      , public filled === Map.fromList [(playerID p, expected p) | p <- roster]
      ]

-- Replacing every private number cannot affect a trading snapshot.
prop_portfoliosKeepSecrets :: Property
prop_portfoliosKeepSecrets = forAll genEngine $ \engine ->
  forAll (vectorOf (length (players engine)) arbitrary) $ \secrets ->
    exchangeSnapshot simulationStart engine === exchangeSnapshot simulationStart
      (engine { players = zipWith (\p secret -> p { privateNumber = secret }) (players engine) secrets })

-- With one instrument, liquidating at the effective price recovers all cash,
-- including gains and losses from any previously closed positions.
prop_effectivePriceBreakEven :: Property
prop_effectivePriceBreakEven = forAll genRoster $ \roster ->
  forAll (elements [minBound .. maxBound]) $ \asset ->
  forAll (listOf ((,) <$> elements (map playerID roster) <*> genOrder)) $ \orders ->
    let initial = newEngineWithInstruments (Set.singleton asset) simulationStart 60 roster
        final = foldl (\engine (pid, order) -> fst (handleRequest simulationStart pid
          (SubmitOrder order { instrument = asset }) engine)) initial orders
    in conjoin
      [ case Map.lookup asset (portfolioEffectivePrices portfolio) of
          Nothing -> units === 0
          Just price -> conjoin [property (units /= 0), portfolioCash portfolio + fromInteger units * price === 0]
      | portfolio <- Map.elems (portfolios (exchangeSnapshot simulationStart final))
      , let units = Map.findWithDefault 0 asset (portfolioPositions portfolio)]

prop_closure :: Property
prop_closure = forAll genEngine $ \engine ->
  forAll genOrder $ \order ->
    let deadline = closesAt (engineInfo engine)
        closed = advanceTo deadline engine
        later = addUTCTime 100 deadline
        (_, snapshot) = handleRequest deadline (PlayerId 1) GetExchangeState engine
    in conjoin
      [ advanceTo later closed === closed
      , enginePhase closed === Resolved (engineResolutions engine)
      , books (engineBook closed) === Map.map (const []) (books (engineBook engine))
      , players closed === players engine
      , accounts (engineBook closed) === accounts (engineBook engine)
      , executedTrades (engineBook closed) === executedTrades (engineBook engine)
      , handleRequest deadline (PlayerId 1) (SubmitOrder order) engine
          === (closed, Reply (Left GameClosed))
      , handleRequest later (PlayerId 1) (SubmitOrder order { orderQuantity = 0 }) closed
          === (closed, Reply (Left GameClosed))
      , snapshot === Reply ExchangeState
          { gameInfo = engineInfo engine
          , observedAt = deadline
          , gamePhase = Resolved (engineResolutions engine)
          , orderBook = Map.map (const []) (books (engineBook engine))
          , tradeHistory = reverse (executedTrades (engineBook engine))
          , revealedNumbers = []
          , portfolios = portfolios (exchangeSnapshot simulationStart engine)
          }
      ]

-- A settlement request reveals nothing before the deadline and the full roster
-- afterwards, including inactive players and the caller's own result.
prop_settlementDisclosure :: Property
prop_settlementDisclosure = forAll genEngine $ \engine ->
  forAll (elements (players engine)) $ \player ->
    let pid = playerID player
        requestAt now = snd (handleRequest now pid AwaitSettlement engine)
    in conjoin
      [ requestAt (opensAt (engineInfo engine)) === WhenResolved
      , case requestAt (closesAt (engineInfo engine)) of
          Reply result -> conjoin
            [ map settledPlayer (playerResults result) === players engine
            , [playerPayoff entry | entry <- playerResults result, settledPlayer entry == player]
                === [netPayoff result]
            , sum (map playerPayoff (playerResults result)) === 0
            ]
          _ -> counterexample "Settlement unavailable at closure" False
      ]

prop_invalidOrder :: Property
prop_invalidOrder = forAll genEngine $ \engine ->
  forAll genOrder $ \order ->
    forAll (chooseInteger (-100, 0)) $ \quantity ->
      handleRequest simulationStart (PlayerId 1)
        (SubmitOrder order { orderQuantity = quantity }) engine
        === (engine, Reply (Left InvalidQuantity))

-- Relative and absolute waits agree and only change the engine through expiry.
prop_waits :: Property
prop_waits = forAll genEngine $ \engine ->
  forAll (elements (map playerID (players engine))) $ \pid ->
    forAllShrink arbitrary shrink $ \(NonNegative elapsed, seconds) ->
      let now = addUTCTime (fromInteger elapsed) (opensAt (engineInfo engine))
          target = addUTCTime (fromInteger seconds) now
          expected = (advanceTo now engine, ResumeAt (max now target))
      in conjoin
        [ handleRequest now pid (Wait (fromInteger seconds)) engine === expected
        , handleRequest now pid (WaitUntil target) engine === expected
        ]

-- QuickSpec observes the entire engine, including order priority and accounts.
-- Its discovered equations supplement the explicit QuickCheck specifications.
newtype SampleEngine = SampleEngine Engine deriving Show

instance Arbitrary SampleEngine where
  arbitrary = SampleEngine <$> oneof [genEngine, genRevealEngine]

instance QS.Observe () String SampleEngine where
  observe () = show

discoverLaws :: IO ()
discoverLaws = QS.quickSpec
  [ QS.monoTypeObserve (Proxy :: Proxy SampleEngine)
  , QS.con "close" (\(SampleEngine e) -> SampleEngine (advanceTo (closesAt (engineInfo e)) e))
  , QS.withMaxTermSize 3
  , QS.withMaxTests 100
  ]

-- A submission can affect only its instrument, including fills and positions.
prop_instrumentIsolation :: Property
prop_instrumentIsolation = forAll genEngine $ \engine -> forAll genOrder $ \order ->
  forAll (elements (players engine)) $ \player ->
    let (updated, _) = handleRequest simulationStart (playerID player) (SubmitOrder order) engine
        before = engineBook engine
        after = engineBook updated
        asset = instrument order
        newTrades = take (length (executedTrades after) - length (executedTrades before)) (executedTrades after)
    in conjoin
      [ Map.delete asset (books after) === Map.delete asset (books before)
      , Map.map (Map.delete asset . positions) (accounts after)
          === Map.map (Map.delete asset . positions) (accounts before)
      , Map.map (Map.delete asset . netSpent) (accounts after)
          === Map.map (Map.delete asset . netSpent) (accounts before)
      , property (all ((== asset) . tradeInstrument) newTrades)
      ]

-- Disabled requests are inert, including order IDs and public notifications.
prop_enabledInstruments :: Property
prop_enabledInstruments = forAll genRoster $ \roster ->
  forAll (Set.fromList <$> sublistOf [minBound .. maxBound]) $ \enabled ->
    forAll genOrder $ \order ->
      let engine = newEngineWithInstruments enabled simulationStart 60 roster
          (updated, reply) = handleRequest simulationStart (playerID (head roster)) (SubmitOrder order) engine
          closed = advanceTo (closesAt (engineInfo engine)) updated
      in conjoin
        [ Map.keysSet (orderBook (exchangeSnapshot simulationStart updated)) === enabled
        , Map.keysSet (resolutions (settlementFor (playerID (head roster)) closed)) === enabled
        , if instrument order `Set.member` enabled then property (case reply of Reply (Right _) -> True; _ -> False)
          else (updated, reply) === (engine, Reply (Left InstrumentDisabled))
        ]

-- Trading a portfolio pays the sum of trading each instrument independently.
prop_portfolioPayoff :: Property
prop_portfolioPayoff = forAll genRoster $ \roster ->
  forAll (listOf ((,) <$> elements (map playerID roster) <*> genOrder)) $ \orders ->
    let settle enabled tape =
          let initial = newEngineWithInstruments enabled simulationStart 60 roster
              final = foldl (\engine (pid, order) -> fst (handleRequest simulationStart pid (SubmitOrder order) engine)) initial tape
          in map netPayoff (engineSettlements (advanceTo (closesAt (engineInfo final)) final))
        separate = [settle (Set.singleton asset) (filter ((== asset) . instrument . snd) orders)
                   | asset <- Set.toAscList allInstruments]
    in settle allInstruments orders === map sum (transpose separate)

-- Resolution is insensitive to player order; translating every secret shifts
-- location statistics but leaves range and standard deviation unchanged.
prop_resolutionTranslation :: NonEmptyList Integer -> Integer -> Property
prop_resolutionTranslation (NonEmpty values) offset =
  forAll (shuffle values) $ \permuted -> conjoin
    [ conjoin [resolve asset permuted === resolve asset values,
               resolve asset (map (+ offset) values) === resolve asset values + shift asset]
    | asset <- Set.toAscList allInstruments ]
  where
    shift Sum = fromInteger (toInteger (length values) * offset)
    shift Range = 0
    shift StdDev = 0
    shift _ = fromInteger offset

-- Pairwise squared distances characterize population variance without a mean.
-- The rounded result must lie within half a millionth of its square root.
prop_populationStdDev :: NonEmptyList Integer -> Property
prop_populationStdDev (NonEmpty values) =
  let count = toInteger (length values)
      variance = sum [(x - y) ^ (2 :: Int) | x <- values, y <- values] % (2 * count * count)
      result = resolve StdDev values
      halfUnit = 1 % 2000000
  in conjoin
    [ property (result >= 0)
    , property (max 0 (result - halfUnit) ^ (2 :: Int) <= variance)
    , property (variance <= (result + halfUnit) ^ (2 :: Int))
    ]

prop_orderStatistics :: NonEmptyList Integer -> Property
prop_orderStatistics (NonEmpty values) =
  let low = resolve Min values
      high = resolve Max values
      middle = resolve Median values
      ordered = sort values
      middleValues = take (if odd (length values) then 1 else 2)
        (drop ((length values - 1) `div` 2) ordered)
  in conjoin
    [ low === fromInteger (head ordered)
    , high === fromInteger (last ordered)
    , resolve Range values === high - low
    , middle === sum middleValues % toInteger (length middleValues)
    , resolve Sum values === fromInteger (sum values)
    ]
