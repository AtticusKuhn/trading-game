{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module EngineTests (engineProperties, discoverLaws) where

import Control.Monad (unless)
import Data.List (foldl')
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
  , prop_priority
  , prop_waits
  , prop_deferredSettlement
  , prop_simulatorContinuesAfterClosure
  ]

prop_conservation :: Property
prop_conservation = forAll genEngine $ \engine ->
  let state = engineBook engine
      balances = map snd (accounts state)
      book = map snd (ownedOrders state)
      buys = [restingPrice o | o <- book, restingSide o == Buy]
      sells = [restingPrice o | o <- book, restingSide o == Sell]
  in conjoin
    [ sum (map fst balances) === 0
    , sum (map snd balances) === 0
    , property (all ((> 0) . remainingQuantity) book)
    , property (and [buy < sell | buy <- buys, sell <- sells])
    ]

prop_closure :: Property
prop_closure = forAll genEngine $ \engine ->
  forAll genOrder $ \order ->
    let deadline = closesAt (engineInfo engine)
        closed = advanceTo deadline engine
        later = addUTCTime 100 deadline
        (_, snapshot) = handleRequest deadline (PlayerId 1) GetExchangeState engine
    in conjoin
      [ advanceTo later closed === closed
      , enginePhase closed === Resolved (engineTotal engine)
      , ownedOrders (engineBook closed) === []
      , accounts (engineBook closed) === accounts (engineBook engine)
      , executedTrades (engineBook closed) === executedTrades (engineBook engine)
      , handleRequest deadline (PlayerId 1) (SubmitOrder order) engine
          === (closed, Reply (Left GameClosed))
      , handleRequest later (PlayerId 1) (SubmitOrder order { orderQuantity = 0 }) closed
          === (closed, Reply (Left GameClosed))
      , snapshot === Reply ExchangeState
          { gameInfo = engineInfo engine
          , observedAt = deadline
          , gamePhase = Resolved (engineTotal engine)
          , orderBook = []
          , tradeHistory = reverse (executedTrades (engineBook engine))
          }
      ]

prop_invalidOrder :: Property
prop_invalidOrder = forAll genEngine $ \engine ->
  forAll genOrder $ \order ->
    forAll (chooseInteger (-100, 0)) $ \quantity ->
      handleRequest simulationStart (PlayerId 1)
        (SubmitOrder order { orderQuantity = quantity }) engine
        === (engine, Reply (Left InvalidQuantity))

-- Best price first, then oldest at that price, always at the resting price.
prop_priority :: Property
prop_priority =
  let initial = newEngine simulationStart 60 [(PlayerId 1, 3), (PlayerId 2, 7), (PlayerId 3, -2)]
      orders =
        [ (PlayerId 2, LimitOrder Sell (Price 5) 2)
        , (PlayerId 3, LimitOrder Sell (Price 4) 1)
        , (PlayerId 3, LimitOrder Sell (Price 5) 3)
        , (PlayerId 1, LimitOrder Buy (Price 6) 4)
        ]
      final = foldl' (\e (pid, order) ->
        fst (handleRequest simulationStart pid (SubmitOrder order) e)) initial orders
      book = engineBook final
  in conjoin
    [ reverse (executedTrades book) ===
        [Trade (Price 4) 1 simulationStart, Trade (Price 5) 2 simulationStart, Trade (Price 5) 1 simulationStart]
    , ownedOrders book === [(PlayerId 3, RestingOrder (OrderId 3) Sell (Price 5) 2)]
    , accounts book === [(PlayerId 1, (4, -19)), (PlayerId 2, (-2, 10)), (PlayerId 3, (-2, 9))]
    , nextOrderId book === 5
    ]

prop_waits :: Property
prop_waits = forAll (chooseInteger (-100, 100)) $ \seconds ->
  let request = Wait (fromInteger seconds)
      target = addUTCTime (fromInteger seconds) simulationStart
      expected = (fixture, ResumeAt (max simulationStart target))
  in conjoin
    [ handleRequest simulationStart (PlayerId 1) request fixture === expected
    , handleRequest simulationStart (PlayerId 1) (WaitUntil target) fixture === expected
    ]

prop_deferredSettlement :: Property
prop_deferredSettlement =
  let submit pid side = fst . handleRequest simulationStart pid
        (SubmitOrder (LimitOrder side (Price 5) 2))
      resting = submit (PlayerId 1) Buy fixture
      (unchanged, parked) = handleRequest simulationStart (PlayerId 1) AwaitSettlement resting
      filled = submit (PlayerId 2) Sell unchanged
      deadline = closesAt (engineInfo fixture)
  in conjoin
    [ parked === WhenResolved
    , unchanged === resting
    , snd (handleRequest deadline (PlayerId 1) AwaitSettlement filled)
        === Reply (Settlement 10 10)
    , engineSettlements (advanceTo deadline filled) === [Settlement 10 10, Settlement 10 (-10)]
    ]

prop_simulatorContinuesAfterClosure :: Property
prop_simulatorContinuesAfterClosure =
  let program = do
        wait 120
        snapshot <- getExchangeState
        rejected <- submitOrder (LimitOrder Buy (Price 5) 1)
        unless (observedAt snapshot == addUTCTime 120 simulationStart
             && gamePhase snapshot == Resolved 7
             && rejected == Left GameClosed) $
          error "simulator lost post-closure activity"
      final = simulatePlayers simulationStart 60 [(PlayerId 1, program, 7)]
  in enginePhase final === Resolved 7

-- QuickSpec observes the entire engine, including order priority and accounts.
-- Its discovered equations supplement the explicit QuickCheck specifications.
newtype SampleEngine = SampleEngine Engine deriving Show

instance Arbitrary SampleEngine where
  arbitrary = SampleEngine <$> genEngine

instance QS.Observe () String SampleEngine where
  observe () = show

discoverLaws :: IO ()
discoverLaws = QS.quickSpec
  [ QS.monoTypeObserve (Proxy :: Proxy SampleEngine)
  , QS.con "close" (\(SampleEngine e) -> SampleEngine (advanceTo (closesAt (engineInfo e)) e))
  , QS.withMaxTermSize 3
  , QS.withMaxTests 100
  ]
