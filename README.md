# Trading Game

Players use the same `Eff (TradingGame ': effs) ()` programs in virtual and real
time. Each player knows their own private number and trades contracts on the sum
of all private numbers. The default game lasts one hour.

```haskell
{-# LANGUAGE DataKinds #-}

import Control.Effect (run, runIO)
import Control.Monad (void)
import TradingGame

programs :: [PlayerProgram effs]
programs =
  [ (Player (PlayerId 42) "alice" 3, void (submitOrder (LimitOrder Buy  (Price 5) 2)))
  , (Player (PlayerId (-7)) "bob" 7, void (submitOrder (LimitOrder Sell (Price 5) 2)))
  ]

-- Pure; advances virtual time directly to the next event.
simulated :: [Settlement]
simulated = run (runTradingGameFor 10 programs)

-- IO; closes after ten seconds of elapsed real time.
live :: IO [Settlement]
live = runIO (runConcurrent (runLiveFor 10 programs))
```

`runTradingGame` and `runLive` use the default one-hour duration. All settlement
lists follow the input player order. Supply at least one player, with unique IDs and nonblank, unique display names.
Nonpositive durations close immediately.

## Web debugging prototype

```sh
nix run path:.#web
# Custom port and game duration in seconds (default: 3000, 3600):
nix run path:.#web -- 3000 60
```

Open `http://127.0.0.1:3000`, select a player, and place buy or sell limit orders.
The page shows your private number, open buys and sells, the latest twenty
trades, and your payoff at settlement. Prices accept integers, exact decimals,
and fractions. An accepted order can remain open until another order matches.

The roster is fixed at startup by `webPlayerNames` in `TradingGame.Web`:
`alice`, `bob`, `carol`, `dan`, `eve`, `fred`, `gwen`, `hal`, `market-maker`, and
`noise-trader`. The last two players also run bot programs. Each player receives
a private number between 1 and 9 at startup; every player contributes to the sum,
even before anyone joins. Joining selects an existing player by exact,
case-sensitive name. Unknown names return an HTTP 400 error and never create
players. Rejoining from the same or another browser uses the same private number,
orders, positions, and settlement.

Use separate browser profiles/private windows for different players. An opaque
HttpOnly cookie remembers the selected player across refreshes. Names select
identities in this trusted local demo; they are not authentication credentials.
The clock starts at server startup, and restarting resets all state. The server
binds to loopback for local debugging.

One bot replenishes a small two-sided book every two seconds, estimating the sum
from its own private number; the other alternates buying and selling at the best
available quotes every three seconds. Both use the existing trading effects and
stop at settlement. The server remains available to inspect the final game.

`TradingGame.Web` renders Blaze HTML and uses WAI/Warp. The order form posts with
HTMX; the server pushes rendered HTML directly through the
[HTMX SSE extension](https://htmx.org/extensions/sse/). Exchange changes wake the
streams through STM, with keep-alive comments every fifteen seconds and a full
snapshot on reconnect. Updates leave the order form intact. There is no client
polling or custom JavaScript. HTMX, its SSE extension, and the development-only
[Tailwind browser build](https://tailwindcss.com/docs/installation/play-cdn) load
from pinned CDN URLs, so the browser needs internet access.

The terminal entrypoint below remains available. `nix flake check path:.` also
builds the web executable and runs QuickCheck properties covering HTTP order
gating, identity binding, concurrent joins, account continuity, unknown-name rejection,
and SSE framing.

## Terminal prototype

Run against a passive player offering ten contracts at a bid of 9 and an ask of
11. Join as `alice` first, then read your private number with `private`.
The fixed demo roster contains `alice` and `market-maker`.

```sh
# Virtual time; waiting and settlement advance the simulation immediately.
nix run path:.#terminal

# Real time; close after 60 seconds (or supply another integer duration).
nix run path:.#terminal -- live 60
```

Commands:

```text
join alice
private
book
buy 11 2
sell 9 1
wait 5
settlement
help
logout
join alice
quit
```

Prices and seconds accept integers, exact decimals, and fractions such as `3/2`.
Quantities must be positive integers. Invalid input prints an error and retries;
EOF acts as `quit`. `logout` returns to the session prompt; `quit` exits the
terminal. `settlement` waits for closure and prints the current player's payoff.
The live exchange closes at its deadline even while the terminal is waiting for
input; the terminal remains available to inspect the resolved game until quit.

The terminal is a debugging adapter in `TradingGame.Terminal`, with its
executable in `TerminalMain.hs`. Its simulation mode advances a virtual clock
on waits and settlement, with one active caller and a pre-seeded passive market
maker. General multi-program simulations use the event-queue `runTradingGame`.

## Players and sessions

`Player` is a host-only record containing `playerID :: PlayerId`,
`displayName :: String`, and `privateNumber :: Integer`. `newEngine` accepts a
fixed `[Player]`, stored as `players`; trading and session changes preserve it.
Public exchange snapshots do not include the roster or other players' secrets.

```haskell
data PlayerSession :: Effect where
  JoinGameAsPlayer :: String -> PlayerSession m LoginResult
  Logout :: PlayerSession m LogoutResult
  GetCurrentPlayer :: PlayerSession m (Maybe PlayerId)

runPlayerSession :: [Player] -> Eff (PlayerSession ': effs) a -> Eff effs a

runWithCurrentPlayer
  :: (PlayerSession :< effs, IOE :< effs)
  => LiveRuntime
  -> Eff (TradingGame ': effs) a
  -> Eff effs (Either SessionError a)
```

Install one `runPlayerSession (players initialEngine)` handler per connection.
It starts logged out. Names match exactly and case-sensitively; unknown names
return `UnknownPlayerName` and never create a player. Login returns
`Right playerID`; repeated login as the same player is idempotent. Changing
players requires logout first. Logout returns `Right ()`, or `Left NotLoggedIn`
if already logged out. Failed logins leave the session unchanged.

The session is the outer layer. `runWithCurrentPlayer runtime action` checks
login and runtime membership before executing any part of the inner action,
then binds that action to the selected player. It returns `Left NotLoggedIn` or
`Left (UnknownPlayerId pid)` for invalid sessions. Rejoining retains the same
private number, orders, positions, and settlement. Names are identity selectors
for this trusted demo, not authentication credentials.

`terminalSession` owns the join/logout loop and invokes `runWithCurrentPlayer`
only after successful login. The live terminal runs `runExchange` as a scoped
background worker so closure happens even while logged out. The interpreter's
settlement request can also drive deadline closure itself; concurrent callers
share the same engine and final publication.

## Player interaction and effect composition

`TradingGame.Interaction` defines transport-independent `PlayerCommand` and
`PlayerInfo` types and this effect:

```haskell
data PlayerInteraction :: Effect where
  ReadInput :: PlayerInteraction m PlayerCommand
  SendInfo  :: PlayerInfo -> PlayerInteraction m ()
```

`interactivePlayer` reads commands, executes them through `TradingGame`, and
sends typed replies until `Quit` or `LeaveGame` (`logout`). Its polymorphic signature specializes to
`Eff '[TradingGame, PlayerInteraction] ()`. It never reads stdin or prints
anything itself. `runTerminal` is one handler; a scripted handler or a future
web connection can implement the same operations.

Players, steps, and simulator events carry the remaining effects:

```haskell
type PlayerProgram effs = (Player, Eff (TradingGame ': effs) ())

stepPlayer
  :: Eff (TradingGame ': effs) ()
  -> Eff effs (PlayerStep effs)

runTradingGameFor
  :: NominalDiffTime -> [PlayerProgram effs] -> Eff effs [Settlement]

runLivePlayer
  :: IOE :< effs
  => UTCTime -> LiveRuntime -> PlayerProgram effs -> Eff effs ()
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

Programmatic players can still use `interactivePlayer` with scripted interaction
handlers around the simulator or live runner. For a human terminal, compose
`runTerminal`, `runPlayerSession`, and `terminalSession runtime` as shown in
`TerminalMain.hs`; this keeps joining outside the in-game command loop.

The pinned `eff` revision has no public IO-unlifting API. `TradingGame.Concurrent`
isolates a small bridge using `Control.Effect.Internal`: scoped actions borrow
the current handler environment, with a separate prompt in each thread. Outer
handlers are shared and must synchronize mutable resources when multiple
workers use them. Nonlocal continuation capture or abort across this boundary
is unsupported and raises an explicit IO error; install such a handler inside
the worker action instead. Ordinary request/reply handlers, including the
terminal adapter, work outside the scope. The bridge depends on the pinned
library internals and should be reviewed if that dependency changes.

Migration: the old `(PlayerId, program, Int)` tuple is now
`(Player playerId name secret, program) :: PlayerProgram effs`. Secrets use
`Integer`. `newEngine` takes `[Player]` instead of ID/secret pairs, and
`engineSecrets` is replaced by `players`. Simulation and live entry points still
return `Eff effs`; use `run` for pure simulations and `runIO . runConcurrent`
for ordinary live runs.

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

`TradingGame` re-exports these modules, `Interaction`, `Session`, and `Concurrent`. The engine,
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
    programs
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
Session properties check login identity, logout, connection isolation, unknown
names, account continuity across rejoins, command gating, and timed settlement.
Concurrency properties check cleanup on body return, worker failure, parent
cancellation, nested scopes, and the nonlocal-control boundary. Nix also builds
and smoke-tests the terminal executable.

Generated inputs are shrinkable; each IO case creates a fresh runtime.
Concurrency tests use synchronization barriers; timeouts only guard against
deadlocks. The real-clock property generates short positive durations to keep
the suite fast.
