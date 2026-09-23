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


## Terminal UI
The terminal UI and web UI should share 99% of business logic and share the same behavior, but differ in how each 
view is rendered.
The terminal UI is just for quick debugging. The terminal UI does not have to be pretty, and it can be a 
quick-and-dirty ASCII interface for debugging.

## Web UI
The Web UI is more intended for end-users.
Use SSE instead of polling on the client side. 
The server should communicate by sending HTML, not JSON to the client.
Try not to use too much client-side JS, but rely on HTMX as much as possible.

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
  
  
-- The list of players should be a per-game static constant which is established at the beginning of the game and does not change over the course of the game. You cannot "dynamically add" a new player during a running game.
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

The simulator is deterministic, and runs in virtual time in a single thread. The simulator is used for testing
correctness. If an effect blocks in the simulator, then the entire simulator blocks, but this is intended behavior
and not a bug, because in testing we have custom effect handlers that do not block.



# Testing Strategy
do NOT write glorified unit tests.
Write genuinely property-based tests.
Tests should not be long.
They should not be re-enacting long scenarios, just simple properties.
Only write tests that John Hughes would approve of.
If a test has a hardcoded constant in it, that's code-smell that this
test is potentially just a unit test in disguise, and not a true
property-based quickcheck property.


# Communication Strategy

When communicating with the user, do not write text that is
verbose in implementation details ("wall-of-text" writing style): this was changed to that, these things were split,
those things were merged, this was left untouched, tests were added for this, and so on.
When communicating with the user, instead prioritize saying what is actually important: Why are we doing this? What is the value? How risky or urgent is this work? Where do you want my input? What should I pay attention to? 

# Storing Data
Right now, we're just storing everything in 
in-memory data structures for simplicity. 
We may add a database (e.g. SQLite) in the future,
but not yet.

# Tradable Instruments

```haskell
data Instrument  = Sum | Range | Min | Max | Median | StdDev -- StdDev is population standard deviation, not sample standard deviation.
    deriving (Eq, Ord, Show, Enum, Bounded)

  type Positions   = Map Instrument Integer

resolve :: Instrument -> [Integer] -> Rational
resolve Sum = sum
resolve Max = maxmimum
resolve Min = minimum

```

A player’s final payoff becomes:

`cash + Σ(position[instrument] × resolution[instrument])`

Note that each instrument resolves on its own independent order-book, so for example, a sell of `Sum` would 
not resolve with buy of `Range`.


# Limits 
All players start off with `0` cash, but
there are no limits on the amount of buying or
selling. A player may have negative cash and 
still trade.



# Market Events & Number Reveals
At certain points in the game, 
a private number of a player is publically
revealed, representing a market-event.

The number to be revealed is chosen randomly
with replacement, meaning that the same 
number can be chosen multiple times.

Each player's number could be chosen to be revealed
with equal probability, even a bot player's number.

Sampling must choose uniformly from the full, fixed player roster, including bots and humans. if several players have the same number, that value should be correspondingly more likely.

