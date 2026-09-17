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
- `TradingGame.Live`: concurrent player workers apply requests directly under an
  `MVar Engine` lock. A deadline task closes idle games; one shared `TMVar Engine`
  publishes the final state to settlement waiters.

`TradingGame` re-exports these modules. The engine, low-level runtime API, and
trace hook are host-only; player programs receive only the `TradingGame` effect.

An order's authoritative time is sampled after acquiring the engine lock.
Orders processed at or after closure cannot trade, including callers that started
waiting for the lock earlier. Concurrent runs may have different processing
orders and outcomes; the same ordered requests and timestamps obey identical
rules in both runners. Every request checks the deadline, so busy players cannot
continue trading past closure.

`runLive` returns final settlements at closure and cancels remaining workers.
Settlement waiters share the final engine, but execution of player continuations
after `awaitSettlement` is not guaranteed. Worker, clock, or trace-hook exceptions
abort the run and cancel the workers. Player programs are trusted, interruptible
Haskell computations; OS scheduling and computation can delay notification of
closure.

## Configuring and testing live execution

```haskell
configured :: IO [Settlement]
configured = do
  clock <- newLiveClock
  runLiveWith clock
    defaultLiveConfig { liveDuration = 10 }
    players
```

`LiveClock` injects both the current time and an STM deadline signal. The real
clock anchors UTC once and measures elapsed time with a monotonic clock.
`LiveTests.manualClock` demonstrates a test clock backed by a single `TVar`:
advancing it wakes timers without sleeping.
Build live executables with GHC's `-threaded` option for the real clock's timers.

`onLiveEvent` optionally records processed requests, their timestamps and
decisions, and closure. The hook runs under the engine lock: keep it short and do
not call back into the runtime. Deferred settlement requests are traced once as
`WhenResolved`; closure publishes the shared final engine without per-player
reply events. Traces can contain private-number replies and belong to the host.
`runLiveEngineWith` exposes the final engine for host inspection. Tests replay
live traces through the pure engine and compare responses and final state.

The high-level runners retain their signatures. The host-only runtime API now
uses `newLiveRuntime clock trace initial`, `requestLive runtime pid request`, and
`runExchange runtime`. `requestLive` returns a `Decision` directly; `WhenResolved`
asks the caller to wait on `runtimeFinal` outside the engine lock. Queue capacity,
`Request`, and `LiveFailure`/`LiveStopped` have been removed. Calls made directly
to a resolved runtime still obey the core rules, including `GameClosed` for orders;
low-level callers manage their own task lifetimes and exception supervision.

Run the suite or the Nix check with the current working tree, including new files:

```sh
nix run path:. -- +RTS -N2 -RTS
nix flake check path:.
```

The suite includes QuickCheck rule properties, QuickSpec equation discovery,
live trace replay, concurrent order updates, manually timed players, shared
settlement notification, idle closure, deadlines under lock contention, lock
recovery, exception supervision, and a real-clock smoke test.
All live scenarios run as QuickCheck IO properties with generated, shrinkable
inputs: player IDs and secrets, prices and quantities, clock offsets and wait
intervals, and concurrent order counts. Each case creates a fresh runtime.
Concurrency tests use synchronization barriers; timeouts only guard against
deadlocks. The real-clock property generates short positive durations to keep
the suite fast.
