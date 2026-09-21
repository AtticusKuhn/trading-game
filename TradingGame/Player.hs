{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module TradingGame.Player where

import Control.Effect (Eff, control0, handle)
import TradingGame.Core

type Player effs = (PlayerId, Eff (TradingGame ': effs) (), Int)

-- Each runner uses the same evaluator. Continuations retain player-local state.
data PlayerStep effs where
  Finished :: PlayerStep effs
  Requested :: TradingGame m a -> (a -> Eff (TradingGame ': effs) ()) -> PlayerStep effs

stepPlayer :: Eff (TradingGame ': effs) () -> Eff effs (PlayerStep effs)
stepPlayer = handle (\() -> pure Finished)
  (\request -> control0 $ \resume -> pure (Requested request resume))

