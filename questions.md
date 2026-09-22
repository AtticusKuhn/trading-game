
I'm trying to brainstorm how to modify the existing
trading game to support a webUI for players
to interact with.

I think players should submit orders with POST
requests and receive market data with SSE.

Can you brainstorm several ways to accomplish this? 
How would the technical architecture have to change? 
Do not make any changes yet.



I want to explore a simple prototype of adding player interactivity.⠄It can even be⠐in the
  terminal for now. I want to add a new effect called something "PlayerInteraction" or
  something like that which is an effect that requests input from the user and can also send
  info to⠁the user. With this, make a tradinggame program⠐that has both 
`Eff '[TraingGame, PlayerInteraction] ()` that just reads a command from the user and executes⡀it.
  
Can you sketch out how this would look? 
Would this be a good or a bad architecture ?
How would you do it? 
Do not implement yet.

I want to explore adding a `runLive` function 
to TradingGame, that is analogous to `simulate`, 
except that it runs the trading simulation in 
real-time rather than virtual time. I also want 
to maximize code-sharing between them so that
there is no duplication of logic
Can you sketch out how this would look? 
Would this be a good or a bad architecture ?
How would you do it? 
Do not implement yet.
What if I wanted to have `runLive` use multithreading/concurrency with STM? Would this be a
  good idea or not? 

  The important distinction is that both runners should implement identical trading rules.
  current simulate combines three responsibilities:
  1. Evaluating player programs until their next request.
  2. Handling requests, including matching, snapshots, and settlement.
  3. Scheduling continuations and advancing time.
  
Use TBQueue Request queue. Each player gets exactly one thread.
Concurrent live runs can have different outcomes because of nondeterminacy, but that's not a concern.
The simulator honors waits beyond closure and runs until player activity finishes. `runLive` doesn't have to preserve this behavior. It should just do whatever is simplest to implement.
Would this be testable or not? 
How should I architect it, assuming that I want it to be testable? 

I was wondering if I should refactor
`data TradingGame :: Effect where`
into two effects: one for trading and one
which is `TimeClock` effect that handles
scheduling, callbacks, waiting, getCurrentTime.
Would this make the code better or worse? 
What would be the ramifications? 






I want to explore a simple prototype of adding player interactivity.⠄It can even be⠐in the
  terminal for now. I want to add a new effect called something "PlayerInteraction" or
  something like that which is an effect that requests input from the user and can also send
  info to⠁the user. With this, make a tradinggame program⠐that has both 
`Eff '[TraingGame, PlayerInteraction] ()` that just reads a command from the user and executes⡀it.
  
It would look something like 

```haskell
data PlayerInteraction :: Effect where
    ReadInput :: PlayerInteraction m PlayerCommand
    SendInfo  :: PlayerInfo -> PlayerInteraction m ()
```
Then, an interactive player could look like:

```haskell
interactivePlayer :: Eff '[TradingGame, PlayerInteraction] ()
interactivePlayer = loop
    where
      loop = do
        command <- readInput
        case command of
          Quit   -> pure ()
          cmd  -> execute cmd >> loop
```
Also, it would teach the simulator to say that `TradingGame` is one effect, but there may be others
and leave the other effects unhandled.
so it would be something like 

```haskell
stepPlayer
    :: Eff (TradingGame ': effs) ()
    -> Eff effs (PlayerStep effs)
```
and also teach the live runner to leave other effect unhandled. 
The terminal would just be a simple, prototype handler of the `PlayerInteraction` effect,
but there can be others as well.
Do not worry about a blocking terminal read locking up the simulator; that's not a problem because
when we do property-based testing on the simulator, we'll use a different effect handler.
The terminal effect-handler will only for user debugging right now.
I’d generalize the individual live worker to handle only TradingGame, leaving other
  effects unhandled.
Maybe something like 
```haskell
runLivePlayer
    :: IOE :< effs
    => UTCTime
    -> LiveRuntime
    -> Player effs
    -> Eff effs ()
```
Also, I don't want to be forced to handle all effects in the worker thread of run-live, so maybe I should
introduce a new effect for concurrency.
Maybe something like

```haskell
data Concurrent :: Effect where
    WithWorkers :: [m ()] -> m a -> Concurrent m a
```

with a handler like 
```haskell
runConcurrent
    :: IOE :< effs
    => Eff (Concurrent ': effs) a
    -> Eff effs a
```
.

  This signature works:
```haskell 
runLivePlayer
    :: IOE :< effs
    => UTCTime
    -> LiveRuntime
    -> Player effs
    -> Eff effs ()
```
  It handles TradingGame and leaves all other effects untouched. The caller chooses how to handle those remaining effects.
  
Please implement this.
Remember to add a small debugging endpoint with a terminal handler, so that the end-user can 
run the terminal version to debug, but remember that the terminal handler will only be one way 
to handle the effect (with over a web-connection being a possible future extension direction).



Imagine that I wanted to change the⠂rules slightly so that at certain points, the secret number of a random player is⠈revealed publically (representing a market event, or new market    ⠄
  information).
How should I architect this? 
Should I add an ability to callback for the player in `TradingGame :: Effect`, i.e. `callbackOnNewMarketEvent :: (MarketEvent -> )`? Or should I 
leave it to the player to poll for new market events `getListOfMarketEvents`? What would be cleanest? 
How would the architecture of the project have to change? What would be the ramifications?  
Don't make any changes yet.


I'm thinking about how I want to design a prototype of the web version.
Let's say that I want to allow for multiple human players to join the webUI.
How should I architect it? 
I just want to keep it simple. 
Should each player be randomly assigned an ID on join? 
Or should each player choose an ID via a dialog on join? 
How should the ID be stored, and can HTMX support that? 
Don't implement anything yet.
No changes yet.
Just prototyping for now.
I'm thinking about changing the player to be something like
```haskell
data Player = Player {
    playerID :: PlayerID
    displayName :: String
    privateNumber :: Integer
}
data Engine = Engine
  { engineInfo :: GameInfo
  , players :: [Player]
  , enginePhase :: GamePhase
  , engineBook :: !Exchange
  } deriving (Eq, Show)
```

And having a new effect

```haskell
data LoginResult = SuccesfulLogin | InvalidName

data PlayerSession :: Effect where
    JoinGameAsPlayer :: String -> PlayerSession m LoginResult
    Logout :: PlayerSession m LogoutResult
    GetCurrentPlayer :: PlayerSession m (Maybe PlayerId)
```

but I don't know yet.
Don't worry about account security yet; this is still just a prototype.
How can I model that once a player has joined a game that a player can start running `TradingGame` effects? Should I have a constructor of
  `PlayerSession` that's like `runCommand`, but only works if the player is logged in? How should I architect this? Because there are two layers:
  first player must join game, and second player must run in-game commands. The player session is the outer layer, and the running in-game
  sending orders is the inner layer.
The engine must launch with a list of players. The list of players is a static constant throughout the game,
and does not change. If a player select the same name, they should join as the same player, not create a new player.

```haskell
runWithCurrentPlayer
    :: (PlayerSession :< effs, IOE :< effs)
    => LiveRuntime
    -> Eff (TradingGame ': effs) a
    -> Eff effs (Either SessionError a)
```



Can you please edit the game to track the idea of players/sessions?
Here's a sketch of what I mean:

```haskell
data Player = Player {
    playerID :: PlayerID
    displayName :: String
    privateNumber :: Integer
}
data Engine = Engine
  { engineInfo :: GameInfo
  , players :: [Player]
  , enginePhase :: GamePhase
  , engineBook :: !Exchange
  } deriving (Eq, Show)
```

The engine should launch with a list of players. The list of players is a static constant throughout the game,
and does not change. If a player rejoins with the same name, they should join as the same player, not create a new player.

And having a new effect

```haskell
data PlayerSession :: Effect where
    JoinGameAsPlayer :: String -> PlayerSession m LoginResult
    Logout :: PlayerSession m LogoutResult
    GetCurrentPlayer :: PlayerSession m (Maybe PlayerId)
```

And use it with 


```haskell
runWithCurrentPlayer
    :: (PlayerSession :< effs, IOE :< effs)
    => LiveRuntime
    -> Eff (TradingGame ': effs) a
    -> Eff effs (Either SessionError a)
```

Please edit the terminal UI so that the player is first in the `PlayerSession` layer (i.e. must join with a name), and
once they have joined with a name, then they can run commands.

There are two layers:
 first player must join game, and second player must run in-game commands. The player session is the outer layer, and the running in-game
  sending orders is the inner layer.

Please make this refactor.


I currently have the effect `PlayerSession :: Effect`. What would happen if I added a 
constructor `RunCommandAsCurrentPlayer`, for example:

```haskell
data PlayerSession :: Effect where
    JoinGameAsPlayer :: String -> PlayerSession m LoginResult
    Logout :: PlayerSession m LogoutResult
    GetCurrentPlayer :: PlayerSession m (Maybe PlayerId)
    RunAsCurrentPlayer
    ::  TradingGame :< m
    -> PlayerSession m (Either SessionError a)
```

Would this be a good or bad design decision? What would be the ramifications? 



Can you please implement a web UI prototype? 
It doesn't exist yet.
Please make a simple debugging applicaton. Do not remove the terminal entrypoint.
If you need to add BlazeHTML/HTMX to the nix flake, you may do so.
When the player opens the webpage, they should have to join as a player with a name.
Once the player has joined, they can make orders.
Use SSE instead of polling on the client side. 
The server should send HTML, not JSON to the client.
Try not to use too much client-side JS, but rely on HTMX as much as possible.
Have a way to run the web server via the command-line.
In the web-application, the user should be able to see open orders and to make new orders.
The web-application should also have bot-players in it that make trades.
