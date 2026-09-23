module TradingGame.Display (renderNumber, renderHolding) where

import qualified Data.Map.Strict as Map
import Data.Ratio (denominator, numerator)
import TradingGame.Core

renderNumber :: Rational -> String
renderNumber value
  | denominator value == 1 = show (numerator value)
  | otherwise = show (numerator value) ++ "/" ++ show (denominator value)

renderHolding :: PublicPortfolio -> Instrument -> String
renderHolding portfolio asset
  | units == 0 = "0"
  | otherwise = show units ++ maybe "" ((" @ $" ++) . renderNumber)
      (Map.lookup asset (portfolioEffectivePrices portfolio))
  where
    units = Map.findWithDefault 0 asset (portfolioPositions portfolio)
