# Trading Game
 In this⠠game, there will⡀be N⡀players, each of which have a
  private number p_i. Each player can place a buy or a sell on the sum S = sum_i p_i. A
  player can see their own private number, but not any other player's private number. Players
  can only submit⠄buy orders or sell orders for⠄S. The game lasts 1 hour. Players can⠂see at
  any moment the current state of⠁the exchange. At⡀the end of the game, the value of the sum
⠐⢀resolves to S.

# Tech Stack
- Test with quickcheck and quickspec
- Effects with `Eff`
- Use Text.Blaze.HTML for html rendering.
- use tailwindcss for styling
- use HTMX for client-side interactivity (HTMX supports SSE https://htmx.org/extensions/sse/)
- use Wai.Warp for web server.

# Ways of Running
Should be flexible enough to run either as terminal CLI program or as web UI.

# Effects
```haskell
data Concurrent :: Effect where
  WithWorkers :: [m ()] -> m a -> Concurrent m a


data PlayerInteraction :: Effect where
  ReadInput :: PlayerInteraction m PlayerCommand
  SendInfo :: PlayerInfo -> PlayerInteraction m ()


data TradingGame :: Effect where
  GetMyPrivateNumber :: TradingGame m Integer
  GetExchangeState :: TradingGame m ExchangeState
  SubmitOrder :: LimitOrder -> TradingGame m OrderResult
  AwaitSettlement :: TradingGame m Settlement
  Wait :: NominalDiffTime -> TradingGame m ()
  WaitUntil :: UTCTime -> TradingGame m ()
  
  

data PlayerSession :: Effect where
    JoinGameAsPlayer :: String -> PlayerSession m LoginResult
    Logout :: PlayerSession m LogoutResult
    GetCurrentPlayer :: PlayerSession m (Maybe PlayerId)
```

# Simulator & Live executor.
2 main entrypoints for running the game

```haskell
-- run an event-queue simulator in virtual time.
runTradingGame :: [Player effs] -> Eff effs [Settlement]

-- run concurrently each player in own thread in real-time
runLive :: (IOE :< effs, Concurrent :< effs) => [Player effs] -> Eff effs [Settlement]
```

# Testing Strategy
do NOT write glorified unit tests.
Write genuinely property-based tests.
Tests should not be long.
They should not be re-enacting long scenarios, just simple properties.
Only write tests that John Hughes would approve of.

