# Trading Game

Players use the same `Eff (TradingGame ': effs) ()` programs in virtual and real
time. Each player knows their own private number and trades contracts on the sum
of all private numbers. The default game lasts one hour.

```haskell
{-# LANGUAGE DataKinds #-}

import Control.Effect (run, runIO)
import Control.Monad (void)
import TradingGame

players :: [Player effs]
players =
  [ (PlayerId 42, void (submitOrder (LimitOrder Buy  (Price 5) 2)), 3)
  , (PlayerId (-7), void (submitOrder (LimitOrder Sell (Price 5) 2)), 7)
  ]

-- Pure; advances virtual time directly to the next event.
simulated :: [Settlement]
simulated = run (runTradingGameFor 10 players)

-- IO; closes after ten seconds of elapsed real time.
live :: IO [Settlement]
live = runIO (runConcurrent (runLiveFor 10 players))
```

`runTradingGame` and `runLive` use the default one-hour duration. All settlement
lists follow the input player order. Supply at least one player, with unique IDs.
Nonpositive durations close immediately.

## Terminal prototype

Run against a passive player offering ten contracts at a bid of 9 and an ask of
11. Your player can read its own private number with `private`.

```sh
# Virtual time; waiting and settlement advance the simulation immediately.
nix run path:.#terminal

# Real time; close after 60 seconds (or supply another integer duration).
nix run path:.#terminal -- live 60
```

Commands:

```text
private
book
buy 11 2
sell 9 1
wait 5
settlement
help
quit
```

Prices and seconds accept integers, exact decimals, and fractions such as `3/2`.
Quantities must be positive integers. Invalid input prints an error and retries;
EOF acts as `quit`. `settlement` waits for closure. In simulation mode, quitting
finishes your program and lets virtual time advance to closure. In live mode,
quitting ends your player, but the session still waits for its deadline. The
terminal prints your final settlement when the runner returns.

The terminal is a debugging adapter, implemented in `TradingGame.Terminal`, with
its executable in `TerminalMain.hs`. A blocking terminal read can pause the
simulator; pure scripted handlers avoid this when testing. The terminal adapter
is intended for one human player per session.

## Player interaction and effect composition

`TradingGame.Interaction` defines transport-independent `PlayerCommand` and
`PlayerInfo` types and this effect:

```haskell
data PlayerInteraction :: Effect where
  ReadInput :: PlayerInteraction m PlayerCommand
  SendInfo  :: PlayerInfo -> PlayerInteraction m ()
```

`interactivePlayer` reads commands, executes them through `TradingGame`, and
sends typed replies until `Quit`. Its polymorphic signature specializes to
`Eff '[TradingGame, PlayerInteraction] ()`. It never reads stdin or prints
anything itself. `runTerminal` is one handler; a scripted handler or a future
web connection can implement the same operations.

Players, steps, and simulator events carry the remaining effects:

```haskell
type Player effs = (PlayerId, Eff (TradingGame ': effs) (), Int)

stepPlayer
  :: Eff (TradingGame ': effs) ()
  -> Eff effs (PlayerStep effs)

runTradingGameFor
  :: NominalDiffTime -> [Player effs] -> Eff effs [Settlement]

runLivePlayer
  :: IOE :< effs
  => UTCTime -> LiveRuntime -> Player effs -> Eff effs ()
```

`stepPlayer` handles only `TradingGame`. Its continuations retain the rest of the
effect stack. The simulator and individual live worker likewise leave other
effects to the caller. `runLivePlayer` itself does not fork a thread.

The high-level live runners require `Concurrent :< effs` and `IOE :< effs`:

```haskell
data Concurrent :: Effect where
  WithWorkers :: [m ()] -> m a -> Concurrent m a

runConcurrent
  :: IOE :< effs
  => Eff (Concurrent ': effs) a -> Eff effs a
```

`WithWorkers` scopes worker lifetimes to its body. Returning, throwing, or
cancelling the body cancels and joins the workers. Worker exceptions propagate
to the body and cancel sibling workers. Successful workers do not terminate the
body. The live runner uses its exchange deadline task as the body.

The caller can handle interaction around the whole runner:

```haskell
-- The player effect stack is inferred separately in each expression.
simulatedTerminal :: IO [Settlement]
simulatedTerminal = runIO $ runTerminal $
  runTradingGameFor 60 [(PlayerId 1, interactivePlayer, 3)]

liveTerminal :: IO [Settlement]
liveTerminal = runIO $ runTerminal $ runConcurrent $
  runLiveFor 60 [(PlayerId 1, interactivePlayer, 3)]
```

These examples additionally import `TradingGame.Terminal (runTerminal)`.
Handlers need not all be installed inside individual workers.

The pinned `eff` revision has no public IO-unlifting API. `TradingGame.Concurrent`
isolates a small bridge using `Control.Effect.Internal`: scoped actions borrow
the current handler environment, with a separate prompt in each thread. Outer
handlers are shared and must synchronize mutable resources when multiple
workers use them. Nonlocal continuation capture or abort across this boundary
is unsupported and raises an explicit IO error; install such a handler inside
the worker action instead. Ordinary request/reply handlers, including the
terminal adapter, work outside the scope. The bridge depends on the pinned
library internals and should be reviewed if that dependency changes.

Migration from the original API: `Player` now takes an effect-list parameter;
all simulation entry points return `Eff effs`, so wrap effect-free simulations
in `run`. High-level live entry points also return `Eff effs`; wrap ordinary live
runs in `runIO . runConcurrent`, as above.

## Shared rules and separate scheduling

- `TradingGame.Core`: pure `Engine`, `advanceTo`, `handleRequest`, matching,
  snapshots, and settlement. The handler returns `Reply`, `ResumeAt`, or
  `WhenResolved`; it does not run continuations or sleep.
- `TradingGame.Player`: the shared `stepPlayer` evaluator and typed continuations.
- `TradingGame.Simulation`: the virtual event queue. Every trading request yields
  to already runnable players. Waits beyond closure and subsequent player
  activity are preserved; simulation finishes when all player activity finishes.
- `TradingGame.Live`: concurrent player workers apply requests directly under an
  `MVar Engine` lock. A deadline task closes idle games; one shared `TMVar Engine`
  publishes the final state to settlement waiters.

`TradingGame` re-exports these modules, `Interaction`, and `Concurrent`. The engine,
low-level runtime API, and trace hook are host-only. Interaction replies expose
only the caller's private number, public snapshots, order results, and the
caller's settlement.

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
  runIO $ runConcurrent $ runLiveWith clock
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

The host-only runtime API uses `newLiveRuntime clock trace initial`,
`requestLive runtime pid request`, and `runExchange runtime`. `requestLive`
returns a `Decision` directly; `WhenResolved` asks the caller to wait on
`runtimeFinal` outside the engine lock. Calls made directly to a resolved runtime
still obey the core rules, including `GameClosed` for orders. Low-level callers
manage their own task lifetimes and exception supervision.

Run the suite or the Nix checks with the current working tree, including new files:

```sh
nix run path:. -- +RTS -N2 -RTS
nix flake check path:.
```

The suite includes QuickCheck rule properties, QuickSpec equation discovery,
live trace replay, concurrent order updates, manually timed players, shared
settlement notification, idle closure, deadlines under lock contention, lock
recovery, exception supervision, and a real-clock smoke test. Interaction
properties use a pure scripted handler to check command/reply ordering, effect
forwarding, waits after closure, and quitting without consuming more input.
Concurrency properties check cleanup on body return, worker failure, parent
cancellation, nested scopes, and the nonlocal-control boundary. Nix also builds
and smoke-tests the terminal executable.

Generated inputs are shrinkable; each IO case creates a fresh runtime.
Concurrency tests use synchronization barriers; timeouts only guard against
deadlocks. The real-clock property generates short positive durations to keep
the suite fast.
