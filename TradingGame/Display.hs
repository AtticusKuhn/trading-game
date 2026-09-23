module TradingGame.Display (renderNumber, renderHolding) where

import qualified Data.Map.Strict as Map
import TradingGame.Core

renderNumber :: Rational -> String
renderNumber value = sign ++ show whole ++ decimals
  where
    -- Round exactly, without converting potentially large values to Double.
    hundredths = round (value * 100) :: Integer
    (whole, fraction) = abs hundredths `divMod` 100
    sign = if hundredths < 0 then "-" else ""
    decimals
      | fraction == 0 = ""
      | fraction `mod` 10 == 0 = "." ++ show (fraction `div` 10)
      | otherwise = "." ++ (if fraction < 10 then "0" else "") ++ show fraction

renderHolding :: PublicPortfolio -> Instrument -> String
renderHolding portfolio asset
  | units == 0 = "0"
  | otherwise = show units ++ maybe "" ((" @ $" ++) . renderNumber)
      (Map.lookup asset (portfolioEffectivePrices portfolio))
  where
    units = Map.findWithDefault 0 asset (portfolioPositions portfolio)
