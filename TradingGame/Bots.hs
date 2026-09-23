{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module TradingGame.Bots (marketMaker, randomTrader) where

import Control.Effect (Eff, (:<))
import Control.Monad (forM_, void, when)
import qualified Data.Map.Strict as Map
import System.Random (StdGen, randomR)
import TradingGame.Core

-- Plug-in estimates use the caller's secret and the mean of the server's
-- 1..9 distribution for unknown players. These are simple demo strategies.
estimateResolution :: Integer -> ExchangeState -> Instrument -> Rational
estimateResolution secret snapshot asset =
  resolve asset (secret : replicate (playerCount (gameInfo snapshot) - 1) 5)

marketMaker :: TradingGame :< effs => Eff effs ()
marketMaker = do
  secret <- getMyPrivateNumber
  let loop = do
        snapshot <- getExchangeState
        when (gamePhase snapshot == Trading) $ do
          forM_ (Map.toAscList (orderBook snapshot)) $ \(asset, book) ->
            forM_ [Buy, Sell] $ \side ->
              when (not (any ((== side) . restingSide) book)) $
                void (submitOrder (LimitOrder side
                  (Price (estimateResolution secret snapshot asset + if side == Buy then -2 else 2)) asset 5))
          wait 2
          loop
  loop

-- A seed makes the same bot usable in deterministic simulations and live games.
randomTrader :: TradingGame :< effs => StdGen -> Eff effs ()
randomTrader = loop
  where
    loop seed = do
      snapshot <- getExchangeState
      let available = Map.keys (orderBook snapshot)
      when (gamePhase snapshot == Trading && not (null available)) $ do
        let (index, next) = randomR (0, length available - 1) seed
            (buy, next') = randomR (False, True) next
            (offset, next'') = randomR (-10, 10 :: Integer) next'
            (quantity, next''') = randomR (1, 100 :: Integer) next''
            asset = available !! index
        secret <- getMyPrivateNumber
        void (submitOrder (LimitOrder (if buy then Buy else Sell)
          (Price (estimateResolution secret snapshot asset + fromInteger offset)) asset quantity))
        wait 1
        loop next'''
