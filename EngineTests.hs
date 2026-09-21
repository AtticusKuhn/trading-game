{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module EngineTests (engineProperties, discoverLaws) where

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

-- Relative and absolute waits agree and only change the engine through expiry.
prop_waits :: Property
prop_waits = forAll genEngine $ \engine ->
  forAll (elements (map fst (engineSecrets engine))) $ \pid ->
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
