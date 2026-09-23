-- Host-side sampling: one independent draw per event, over roster slots, never
-- over distinct private values or the subset of players currently connected.
module TradingGame.Reveals (sampleRevealTargets) where

import Data.Time.Clock (UTCTime)
import System.Random (RandomGen, randomRs)
import TradingGame.Core (Player(..), PlayerId)

sampleRevealTargets :: RandomGen g => g -> [Player] -> [UTCTime] -> [(UTCTime, PlayerId)]
sampleRevealTargets _ [] _ = error "sampleRevealTargets: empty roster"
sampleRevealTargets seed roster times =
  zip times [playerID (roster !! slot) | slot <- randomRs (0, length roster - 1) seed]
