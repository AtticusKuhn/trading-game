# Trading Game

Players use the same `Eff (TradingGame ': Concurrent ': effs) ()` programs in virtual and real
time. Each player knows their own private number and trades contracts on statistics
of all private numbers: `Sum`, `Range`, `Min`, `Max`, `Median`, and `StdDev`.
The default game lasts one hour. Each instrument has an independent order book;
orders in different instruments never match. Accounts share cash but track a
separate integer position in each instrument. Final payoff is
`cash + sum (position[instrument] * resolution[instrument])`.

Market events reveal private numbers during trading. By default an N-player
game has N reveals, at `start + (end - start) * i / (N + 1)` for `i = 1..N`.
The host samples a player independently for each event, uniformly from the full
fixed roster, including bots and humans who never join. Sampling is with
replacement: players and values can repeat, and a value held by multiple players
has their combined probability. Targets are fixed at creation and kept private.
Public snapshots contain all `GameInfo.revealTimes` and the chronological
`ExchangeState.revealedNumbers`, preserving duplicates.

The creation form offers the default schedule or custom UTC timestamps, one per
line. Each line is one event; an empty custom schedule disables reveals.
Times must be at or after the start and strictly before the end; duplicates are
allowed and inputs are sorted chronologically. Web and terminal exchange updates
show the revealed numbers and full schedule. SSE publishes reveals even when
nobody is trading. Late starts or clock jumps publish every event already due.

`awaitUntilNextReveal` (`AwaitUntilNextReveal`) waits for the first event strictly
after the call, returning `Just number`, or immediately returns `Nothing` when
none remain. It broadcasts the same value to every waiter. Simultaneous events
are published together; the wait returns the first value in that group. Read the
snapshot to obtain the complete history. A delayed live waiter still receives
the event it originally awaited, even if later events have already occurred.

Randomness stays in the host adapter (`TradingGame.Reveals.sampleRevealTargets`),
outside the pure engine. `newEngineWithReveals enabled start duration roster plan`
accepts a host-only `[(UTCTime, PlayerId)]` plan. The older low-level constructors
use an empty plan. The default simulator samples with a fixed seed for repeatable
runs; `runTradingGameWithReveals enabled start duration plan programs` accepts an
explicit plan for tests/replay. Live runners sample fresh targets unless given
`defaultLiveConfig { liveRevealPlan = Just plan }`; `Just []` disables reveals.
`runSimulatedPlayer` preserves pending events across sessions.

Median averages the two middle values for an even roster. `StdDev` is population
standard deviation, rounded to the nearest six decimal places (ties up). All
other resolutions are exact rationals, and standard deviation is calculated
using exact arithmetic before rounding.

```haskell
{-# LANGUAGE DataKinds #-}

import Control.Effect (run, runIO)
import Control.Monad (void)
import TradingGame

programs :: [PlayerProgram effs]
programs =
  [ (Player (PlayerId 42) "alice" 3, void (submitOrder (LimitOrder Buy  (Price 5) Sum 2)))
  , (Player (PlayerId (-7)) "bob" 7, void (submitOrder (LimitOrder Sell (Price 5) Sum 2)))
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
Nonpositive durations close immediately. Each `Settlement` retains the caller's
`netPayoff` and includes `playerResults :: [PlayerResult]` in roster order. Each
result contains `settledPlayer :: Player` (ID, name, and private number) and
`playerPayoff :: Rational`, including players who never traded. These details
are revealed only after resolution.

## Web prototype: concurrent games

```sh
nix run path:.#web
# Custom port and suggested duration for the creation form:
nix run path:.#web -- 3000 60
```

Open `http://127.0.0.1:3000` to see all games, including upcoming and completed
games, and create a game. Choose start/end times **in UTC**, player names, and
human, random trading bot, or market-making bot for each roster entry. Blank
rows are ignored; add more rows with the button. Names must be nonblank and
unique within a game, and the end must be after the start. Instrument checkboxes
default to all enabled; disabled instruments have no book or order choice, and
the server rejects orders for them. Selecting none is allowed. The selection
is fixed for the game, like its roster. All-bot games are
allowed. Games and browser sessions are stored in memory and reset on restart.

The server draws each player's private number uniformly from 1 through 9 at
creation. The complete roster, private numbers, and accounts are then fixed.
Upcoming games cannot be joined or traded in. Their page automatically opens
the join form when their start time arrives. A game with a past start begins
immediately, keeping its original end time; if the end has already passed, it
settles immediately without replaying missed trading time.

Each game has its own `/games/ID/` page, exchange, bots, and browser sessions.
Choose a human roster member to trade. Bot identities cannot be selected by
browsers. Rejoining preserves that player's private number, orders, and account.
The same browser can play different games independently. Use separate browser
profiles/private windows for different players within one game. Names select
identities in this trusted local demo; they are not authentication credentials.
The server binds to loopback.

The game page shows your private number, open orders, recent trades, and final
results. Prices accept integers, exact decimals, and fractions. HTMX submits
orders; SSE sends rendered Blaze HTML for exchange changes, directory updates,
and scheduled starts. Streams send a full snapshot on reconnect and keep-alive
comments every fifteen seconds. There is no client polling or custom JavaScript.
HTMX, its SSE extension, and Tailwind load from pinned CDN URLs.

The market maker estimates each enabled instrument using its secret and the mean
of the private-number distribution for unknown players, then replenishes both
sides of each book. The random trading bot draws enabled instruments, sides,
prices, and quantities from a server-generated seed. Both use the shared
trading effects; seeded bots also work in deterministic simulations. Workers
start when the game opens and are cancelled at its original end time.

## Game management

`TradingGame.ManageGames` provides a transport-independent directory effect:

```haskell
data ManageGames :: Effect where
  CreateNewGame :: NewGameConfig -> ManageGames m (Either CreateGameError GameId)
  LookupGame :: GameId -> ManageGames m (Maybe GameSummary)
  ListAllGames :: ManageGames m [GameSummary]

data NewGameConfig = NewGameConfig
  { gameStart :: UTCTime
  , gameEnd :: UTCTime
  , gameRoster :: [RosterEntry]
  , gameInstruments :: Set Instrument
  , gameRevealTimes :: Maybe [UTCTime]
  }

data RosterEntry = RosterEntry
  { rosterName :: String
  , rosterType :: PlayerType
  }

data PlayerType = HumanPlayer | RandomTradingBot | MarketMakingBot
```

`defaultNewGameConfig start end roster` enables all instruments. Override
`gameRevealTimes` with `Just times` for a custom schedule (`Just []` for none);
`Nothing` selects the default. `configuredRevealTimes` returns the full sorted
public schedule. Override
`gameInstruments` with a `Set Instrument` to choose a subset. Standalone engines
use `newEngineWithInstruments`; simulations accept the same set through
`runTradingGameWithInstruments enabled start duration programs`, and live
runners through `defaultLiveConfig { liveInstruments = enabled }`. Existing
entrypoints default to `allInstruments`. `GameInfo.enabledInstruments` exposes
the fixed selection, and `Settlement.resolutions` contains its final values.

`withGameManager clock` scopes the in-memory directory and all game workers;
`runManageGames manager` interprets the effect. Summaries expose only IDs,
configuration, and directory status (`Upcoming`, `Running`, `Completed`, or
`Failed`), never private numbers. Creation is atomic, including concurrent
requests. A failing game's workers are stopped and its directory entry becomes
unavailable; other games continue. Leaving the manager scope cancels and joins
its workers.

Scheduling is outside the exchange model: no `LiveRuntime` exists before a
game's start. The host-only `gameRuntime` returns nothing for upcoming games and
ensures a due runtime is created exactly once. `Trading`, `Resolved`, and order
errors are unchanged. The configured start/end timestamps are retained even
when creation occurs after the start. Existing standalone simulation/live
entrypoints and the terminal debugging adapter continue to use the same trading
rules. The terminal keeps its small fixed demo; the web adapter uses the manager
to select a runtime.

Property tests cover concurrent creation, time-window boundaries, automatic
start/settlement, fixed rosters, invalid configuration, independent exchanges,
HTTP cookie isolation, future-game request gating, form round trips, and SSE
start notifications. Scheduling tests inject a manual clock rather than sleeping.

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
buy Sum 11 2
sell Sum 9 1
buy Range 4 1
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
terminal. Orders use `buy INSTRUMENT PRICE QUANTITY` or `sell INSTRUMENT PRICE QUANTITY`;
omitting the instrument defaults to `Sum`. `settlement` waits for closure and
prints each enabled instrument’s resolution, your payoff,
and every player's private number and payoff.
The live exchange closes at its deadline even while the terminal is waiting for
input; the terminal remains available to inspect the resolved game until quit.

The terminal is a debugging adapter in `TradingGame.Terminal`, with its
executable in `TerminalMain.hs`. Its simulation mode uses the same deterministic scheduler as `runTradingGame`,
with one active caller and a pre-seeded passive market maker. `runSimulatedPlayer`
pauses virtual time when a session returns and preserves the engine across
logout/rejoin. No OS threads or `runConcurrent` are used in simulation mode.

## Players and sessions

`Player` is a record kept private until settlement, containing `playerID :: PlayerId`,
`displayName :: String`, and `privateNumber :: Integer`. `newEngine` accepts a
fixed `[Player]`, stored as `players`; trading and session changes preserve it.
Public exchange snapshots include scheduled revealed values, without identities,
and keep all future reveal values private.

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

Both the terminal and web adapter run **the same `interactivePlayer`**:

```haskell
interactivePlayer
  :: (TradingGame :< effs, PlayerInteraction :< effs, Concurrent :< effs)
  => Eff effs InteractionExit
```

The command loop returns `QuitApplication` or `LeftGame`, so the terminal's
session layer can distinguish quit from logout without implementing a second
command loop. A scoped worker publishes the initial exchange snapshot, subsequent
changes, and final settlement. It stops after settlement; quitting/logging out
cancels it. The player never reads stdin or renders HTML.

`runTerminal` interprets input/output for the terminal. The web's
`runWebInteraction` interprets the same operations for HTTP command batches and
SSE connections. HTTP order submissions supply `PlaceOrder` followed by `Quit`;
SSE input waits for the final settlement, then supplies `Quit`. SSE output renders
`PlayerInfo` as HTML. Connection failure cancels the player and its worker;
reconnecting starts a fresh scope with a complete snapshot. Browser cookies bind
all these scopes to the same existing account.

```haskell
awaitExchangeChange
  :: TradingGame :< effs => ExchangeState -> Eff effs ExchangeState
```

The supplied snapshot is the last one observed. The request returns immediately
if the public book, trade history, revealed numbers, or phase has changed; otherwise it waits.
Passing the snapshot prevents a lost update between rendering and subscribing.
Time passing alone and rejected orders do not count as changes. Changes can be
coalesced for a slow consumer. After receiving a resolved snapshot, the update
worker fetches settlement and finishes instead of subscribing again.

```haskell
type PlayerProgram effs = (Player, Eff (TradingGame ': Concurrent ': effs) ())

data Concurrent :: Effect where
  WithWorkers :: [m ()] -> m a -> Concurrent m a
```

`runTradingGame` handles both player effects internally, remaining pure when the
other effects are pure. `preparePlayer` translates worker scopes into scheduling
instructions and `stepPlayer` captures their continuations. Children keep their
owner's player ID: they are tasks, never additional participants or accounts.
All runnable tasks take round-robin turns at trading requests. Scope bookkeeping
creates/cancels children without consuming a trading turn, and nested descendants
are cancelled when their scope's body returns. Successful workers do not end the
body. Sleeping and subscribed workers remain suspended until their event occurs.

A task that never reaches a scheduling boundary cannot be preempted. An infinite
sequence of immediate requests can prevent virtual time from advancing. Blocking
input also blocks the simulator. These are intentional cooperative scheduling
semantics. If all remaining tasks await changes after closure, the simulator
reports a deadlock rather than silently dropping their continuations.

Handlers outside the simulator are shared. Handlers captured inside a task
retain their context across requests; `eff`'s local `State` is copied when a
continuation forks, so shared simulation state should be handled outside the
scheduler.

Live execution uses `runConcurrent` and OS threads. The live player installs an
ordinary request interpreter, allowing child workers to trade without capturing
continuations across thread boundaries. `runLivePlayer runtime program` can run
standalone; `runLive` owns the deadline and cancels its players at closure.
Worker exceptions abort the body and cancel sibling workers. Returning, throwing,
or cancelling a body cancels and joins its workers.

The pinned `eff` revision has no public IO-unlifting API. `TradingGame.Concurrent`
uses a small internal bridge with a separate prompt in each thread. Outer live
handlers must synchronize shared mutable resources. Nonlocal continuation capture
or abort across that IO boundary raises an explicit error; install such handlers
inside a worker instead. The pure scheduler also uses `eff`'s internal `Handle`
context to sequence scoped actions, but introduces no IO/thread boundary. Review
these two integration points when upgrading `eff`.

Use `void interactivePlayer` when supplying it as a `PlayerProgram`; session
adapters retain its exit result. Trading-only bots need no behavioral changes,
but explicitly annotated program stacks now include `Concurrent` after
`TradingGame`. Use `run` for pure simulations and `runIO . runConcurrent` for
high-level live runs.

## Shared rules and separate scheduling

- `TradingGame.Core`: pure `Engine`, `advanceTo`, `handleRequest`, matching,
  snapshots, and settlement. The handler returns `Reply`, `ResumeAt`, or
  `WhenResolved`, `WhenRevealed`, or `WhenExchangeChanges`; it does not run continuations or sleep.
- `TradingGame.Player`: scope translation and the pure `stepPlayer` evaluator.
- `TradingGame.Simulation`: the virtual event queue. Every trading request yields
  to already runnable player tasks. Waits beyond closure and subsequent player
  activity are preserved; simulation finishes when all player activity finishes.
- `TradingGame.Live`: concurrent player workers apply requests directly under an
  `MVar Engine` lock. A deadline task closes idle games; one shared `TMVar Engine`
  publishes the final state to settlement waiters.

`TradingGame` re-exports these modules, `Interaction`, `Session`, and `Concurrent`. The engine,
low-level runtime API, and trace hook are host-only. Interaction replies expose
only the caller's private number, public snapshots, order results, and the
caller's settlement, which reveals all players' private numbers and payoffs
after closure.

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
properties compare live and simulated replies, check automatic settlement, and
verify quit/logout results without consuming trailing input. Scheduler properties
check round-robin ordering, unchanged rosters, nested cancellation, local handler
continuations, exchange notifications, and pause/resume equivalence.
Session properties check login identity, logout, connection isolation, unknown
names, account continuity across rejoins, command gating, and timed settlement.
Concurrency properties check cleanup on body return, worker failure, parent
cancellation, nested scopes, and the nonlocal-control boundary. Nix also builds
and smoke-tests the terminal executable.

Generated inputs are shrinkable; each IO case creates a fresh runtime.
Concurrency tests use synchronization barriers; timeouts only guard against
deadlocks. The real-clock property generates short positive durations to keep
the suite fast.
