{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module TradingGame.Bots (marketMaker, randomTrader) where

import Control.Effect (Eff, (:<))
import Control.Monad (void, when)
import System.Random (StdGen, randomR)
import TradingGame.Core

-- Estimate the unknown players at the mean of the server's 1..9 distribution.
marketMaker :: TradingGame :< effs => Eff effs ()
marketMaker = do
  secret <- getMyPrivateNumber
  let loop = do
        snapshot <- getExchangeState
        when (gamePhase snapshot == Trading) $ do
          let estimate = secret + toInteger (playerCount (gameInfo snapshot) - 1) * 5
          mapM_ (quote snapshot estimate) [Buy, Sell]
          wait 2
          loop
      quote snapshot estimate side =
        when (not (any ((== side) . restingSide) (orderBook snapshot))) $
          void (submitOrder (LimitOrder side
            (Price (fromInteger (estimate + if side == Buy then -2 else 2))) 5))
  loop

-- A seed makes the same bot usable in deterministic simulations and live games.
randomTrader :: TradingGame :< effs => StdGen -> Eff effs ()
randomTrader = loop
  where
    loop seed = do
      let (buy, next) = randomR (False, True) seed
          (offset, next') = randomR (-10, 10 :: Integer) next
          (quantity, next'') = randomR (1, 100 :: Integer) next'
      snapshot <- getExchangeState
      when (gamePhase snapshot == Trading) $ do
        secret <- getMyPrivateNumber
        let estimate = secret + toInteger (playerCount (gameInfo snapshot) - 1) * 5
        void (submitOrder (LimitOrder (if buy then Buy else Sell)
          (Price (fromInteger (estimate + offset))) quantity))
        wait 1
        loop next''
