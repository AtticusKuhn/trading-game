# Trading Game

Players use the same `Eff '[TradingGame] ()` programs in virtual and real time.
Each player knows their own private number and trades contracts on the sum of
all private numbers. The default game lasts one hour.

```haskell
{-# LANGUAGE DataKinds #-}

import Control.Monad (void)
import TradingGame

players :: [Player]
players =
  [ (PlayerId 42, void (submitOrder (LimitOrder Buy  (Price 5) 2)), 3)
  , (PlayerId (-7), void (submitOrder (LimitOrder Sell (Price 5) 2)), 7)
  ]

-- Pure; advances virtual time directly to the next event.
simulated :: [Settlement]
simulated = runTradingGameFor 10 players

-- IO; closes after ten seconds of elapsed real time.
live :: IO [Settlement]
live = runLiveFor 10 players
```

`runTradingGame` and `runLive` use the default one-hour duration. All settlement
lists follow the input player order. Supply at least one player, with unique IDs.
Nonpositive durations close immediately.

## Shared rules and separate scheduling

- `TradingGame.Core`: pure `Engine`, `advanceTo`, `handleRequest`, matching,
  snapshots, and settlement. The handler returns `Reply`, `ResumeAt`, or
  `WhenResolved`; it does not run continuations or sleep.
- `TradingGame.Player`: the shared `stepPlayer` evaluator and typed continuations.
- `TradingGame.Simulation`: the virtual event queue. Every request yields to
  already runnable players. Waits beyond closure and subsequent player activity
  are preserved. `simulate` now takes an `Engine` and an event queue; the existing
  `runTradingGame`, `runTradingGameFor`, `runTradingGameAt`, and
  `runTradingGameAt'` entry points retain their signatures.
- `TradingGame.Live`: concurrent player workers, a bounded `TBQueue Request`,
  per-request `TMVar` replies, and one exchange owner. Only that owner applies
  requests to the engine. STM handles communication and shutdown.

`TradingGame` re-exports these modules. The engine, low-level runtime API, and
trace hook are host-only; player programs receive only the `TradingGame` effect.

An order's authoritative time is sampled by the exchange immediately before
handling it. Orders processed at or after closure cannot trade, including orders
that were enqueued earlier. Concurrent runs may have different processing orders
and outcomes; the same ordered requests and timestamps obey identical rules in
both runners. A deadline signal closes an idle exchange, and time checks prevent
a busy queue from delaying closure indefinitely.

`runLive` returns final settlements at closure and cancels remaining workers.
It completes already parked settlement replies, but does not guarantee execution
of player continuations after `awaitSettlement`. Unprocessed queued requests and
blocked queue writers are released with `LiveStopped`, which is a runtime result
distinct from the trading error `GameClosed`. Worker or exchange exceptions abort
the run and trigger cleanup. Player programs are trusted, interruptible Haskell
computations; OS scheduling and computation can delay notification of closure.

## Configuring and testing live execution

```haskell
configured :: IO [Settlement]
configured = do
  clock <- newLiveClock
  runLiveWith clock
    defaultLiveConfig { liveDuration = 10, liveQueueCapacity = 32 }
    players
```

`LiveClock` injects both the current time and an STM deadline signal. The real
clock anchors UTC once and measures elapsed time with a monotonic clock.
`LiveTests.manualClock` demonstrates a test clock backed by a single `TVar`:
advancing it wakes timers without sleeping. Queue capacity must be positive.
Build live executables with GHC's `-threaded` option for the real clock's timers.

`onLiveEvent` optionally records processed requests, their timestamps and
decisions, and closure. The hook runs synchronously on the exchange thread, so
keep it short. Traces can contain private-number replies and belong to the host.
`runLiveEngineWith` also exposes the final engine for host inspection. Tests replay
live traces through the pure engine and compare responses and final state.

Run the suite or the Nix check with the current working tree, including new files:

```sh
nix run path:. -- +RTS -N2 -RTS
nix flake check path:.
```

The suite includes QuickCheck rule properties, QuickSpec equation discovery,
live trace replay, manually timed concurrent players, closure with idle and full
queues, deadline races, exception supervision, and a real-clock smoke test.
Concurrency tests use synchronization barriers; timeouts only guard against
deadlocks.
