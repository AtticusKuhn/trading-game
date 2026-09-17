{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module TradingGame.Player where

import Control.Effect (Eff, control0, handle, run)
import TradingGame.Core

type Player = (PlayerId, Eff '[TradingGame] (), Int)

-- Each runner uses the same evaluator. Continuations retain player-local state.
data PlayerStep where
  Finished :: PlayerStep
  Requested :: TradingGame m a -> (a -> Eff '[TradingGame] ()) -> PlayerStep

stepPlayer :: Eff '[TradingGame] () -> PlayerStep
stepPlayer = run . handle (\() -> pure Finished)
  (\request -> control0 $ \resume -> pure (Requested request resume))

