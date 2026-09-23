{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- A local debugging adapter. The fixed roster belongs to the host; browser
-- names select existing players and opaque cookies identify browser sessions.
module TradingGame.Web where

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVarMasked, newMVar, withMVar, readMVar)
import Control.Exception (bracket, mask_, onException)
import Control.Concurrent.STM
import Control.Effect (Eff, IOE, interpret, liftIO, runIO)
import Control.Monad (forM_, forever, void)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Builder (Builder, byteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isSpace)
import Data.List (find, sortOn)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Text.Read (readMaybe)
import Network.HTTP.Types
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import Numeric (showHex)
import System.Entropy (getEntropy)
import Text.Blaze.Html5 (Html, (!), toHtml, customAttribute)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import TradingGame
import TradingGame.Display (renderHolding, renderNumber)
import TradingGame.Terminal (parseCommand)
import Web.Cookie (parseCookies)

newtype BrowserSession = BrowserSession
  { sessionPlayer :: Player
  }

data WebGame = WebGame
  { webRuntime :: LiveRuntime
  , webSessions :: TVar (Map.Map BS.ByteString BrowserSession)
  , webPlayers :: [Player]
  , webBots :: [Player]
  , webHumans :: Map.Map PlayerId WebPlayer
  }

-- One inbox and worker per human, shared by every browser session and tab.
-- Each order carries its own reply slot; abandoning an HTTP request never
-- blocks the player or hands its reply to another request.
data WebPlayer = WebPlayer
  { webOrders :: TQueue (LimitOrder, TMVar OrderResult)
  , webUpdates :: TChan PlayerInfo
  , webLatest :: TVar (Maybe ExchangeState, Maybe Settlement)
  , webWorker :: Async.Async ()
  }

newWebGameFromRuntime :: LiveRuntime -> [Player] -> [Player] -> IO WebGame
newWebGameFromRuntime runtime roster bots = mask_ $ do
  sessions <- newTVarIO Map.empty
  humans <- start [player | player <- roster, playerID player `notElem` map playerID bots]
  pure (WebGame runtime sessions roster bots (Map.fromList humans))
  where
    start [] = pure []
    start (player:rest) = do
      orders <- newTQueueIO
      updates <- newBroadcastTChanIO
      latest <- newTVarIO (Nothing, Nothing)
      reply <- newEmptyTMVarIO
      let input = atomically $ do
            (order, destination) <- readTQueue orders
            putTMVar reply destination
            pure (PlaceOrder order)
          output info = atomically $ case info of
            OrderSubmitted result -> takeTMVar reply >>= (`putTMVar` result)
            ExchangeSnapshot snapshot -> do
              modifyTVar' latest (\(_, settled) -> (Just snapshot, settled))
              writeTChan updates info
            PlayerSettlement settled -> do
              modifyTVar' latest (\(snapshot, _) -> (snapshot, Just settled))
              writeTChan updates info
            _ -> pure ()
      worker <- Async.asyncWithUnmask $ \unmask -> unmask $ runIO $
        interpret (\request -> case request of
          ReadInput -> liftIO input
          SendInfo info -> liftIO (output info)) $
          runLivePlayer runtime (player, void interactivePlayer)
      remaining <- start rest `onException` Async.cancel worker
      pure ((playerID player, WebPlayer orders updates latest worker):remaining)

-- Keep players available after settlement for closed-order replies and SSE
-- reconnects. The web host owns their lifetime and cancels/joins them on exit.
closeWebGame :: WebGame -> IO ()
closeWebGame = mapM_ (Async.cancel . webWorker) . Map.elems . webHumans

withWebGame :: LiveRuntime -> [Player] -> [Player] -> (WebGame -> IO a) -> IO a
withWebGame runtime roster bots = bracket (newWebGameFromRuntime runtime roster bots) closeWebGame

webPlayer :: WebGame -> BrowserSession -> WebPlayer
webPlayer game session = webHumans game Map.! playerID (sessionPlayer session)

-- Wake callers if the owning worker fails or is cancelled during shutdown.
playerStopped :: WebPlayer -> STM a
playerStopped player = do
  Async.waitSTM (webWorker player)
  throwSTM (userError "Web player stopped")

submitWebOrder :: WebGame -> BrowserSession -> LimitOrder -> IO OrderResult
submitWebOrder game session order = do
  reply <- newEmptyTMVarIO
  let player = webPlayer game session
  atomically (writeTQueue (webOrders player) (order, reply))
  atomically (takeTMVar reply `orElse` playerStopped player)

-- Read-only page queries use the existing session and trading interpreters.
-- Identity always comes from the server's cookie table, never form fields.
asPlayer :: WebGame -> BrowserSession -> Eff '[TradingGame, Concurrent, PlayerSession, IOE] a -> IO a
asPlayer game session action = do
  result <- runIO $ runPlayerSession (webPlayers game) $ do
    void (joinGameAsPlayer (displayName (sessionPlayer session)))
    runConcurrent (runWithCurrentPlayer (webRuntime game) action)
  either (ioError . userError . show) pure result

lookupSession :: WebGame -> Request -> IO (Maybe BrowserSession)
lookupSession game request = do
  sessions <- readTVarIO (webSessions game)
  pure $ do
    cookies <- lookup hCookie (requestHeaders request)
    token <- lookup "trading-session" (parseCookies cookies)
    Map.lookup token sessions

-- Joining creates only a browser session, never a player or a new account.
-- Exact names match the terminal's PlayerSession semantics, including rejoins.
joinWebPlayer :: WebGame -> T.Text -> IO (Either T.Text BS.ByteString)
joinWebPlayer game name = case find ((== T.unpack name) . displayName) (humanPlayers game) of
  Nothing -> pure (Left "Unknown player. Choose a player from this game's roster.")
  Just player -> do
    bytes <- getEntropy 32
    let token = B.pack (concatMap (\b -> let h = showHex b "" in replicate (2 - length h) '0' ++ h) (BS.unpack bytes))
    atomically $ modifyTVar' (webSessions game) (Map.insert token (BrowserSession player))
    pure (Right token)

-- Keep form parsing bounded and exact, reusing the terminal's numeric grammar.
-- Each field must be a single token, so extra commands cannot be smuggled in.
parseOrderForm :: [(BS.ByteString, BS.ByteString)] -> Either String LimitOrder
parseOrderForm fields = do
  values <- traverse field ["side", "instrument", "price", "quantity"]
  case parseCommand (unwords values) of
    Right (PlaceOrder order) -> Right order
    _ -> Left "Choose buy or sell, an instrument, a numeric price, and a positive integer quantity."
  where
    field key = case lookup key fields of
      Just value | not (B.null value) && B.length value <= 100 && not (B.any isSpace value) -> Right (B.unpack value)
      _ -> Left "Complete every order field with a single value."

readForm :: Request -> IO (Either String [(BS.ByteString, BS.ByteString)])
readForm request = go 0 []
  where
    go size chunks = do
      chunk <- getRequestBodyChunk request
      let next = size + BS.length chunk
      if next > 4096 then pure (Left "Form is too large.")
      else if BS.null chunk then pure (Right (parseSimpleQuery (BS.concat (reverse chunks))))
      else go next (chunk:chunks)

htmlResponse :: Status -> ResponseHeaders -> Html -> Response
htmlResponse status headers html = responseLBS status
  ([(hContentType, "text/html; charset=utf-8"), (hCacheControl, "no-store")] ++ headers) (renderHtml html)

webApplication :: WebGame -> Application
webApplication = gameApplication ""

gameApplication :: T.Text -> WebGame -> Application
gameApplication base game request respond = do
  session <- lookupSession game request
  let page content = respond (htmlResponse status200 [] (document $ do
        H.p ! A.class_ "text-sm font-semibold text-slate-600" $
          toHtml (if T.null base then "" else "Game " <> T.takeWhileEnd (/= '/') base)
        content))
      feedback message = respond (htmlResponse status200 [] (H.p ! A.role "status" $ toHtml message))
      redirect headers = respond (htmlResponse status303 ((hLocation, T.encodeUtf8 (base <> "/")):headers) mempty)
  case (requestMethod request, pathInfo request) of
    ("GET", []) -> case session of
      Nothing -> page (joinView base (humanPlayers game) Nothing)
      Just current -> do
        (secret, snapshot, settlement) <- playerSnapshot game current
        page (gameView base current secret snapshot settlement)
    ("POST", ["join"]) -> do
      form <- readForm request
      result <- case form of
        Left problem -> pure (Left (T.pack problem))
        Right fields -> case lookup "name" fields >>= either (const Nothing) Just . T.decodeUtf8' of
          Nothing -> pure (Left "Enter a valid player name.")
          Just name -> joinWebPlayer game name
      case result of
        Left problem -> respond (htmlResponse status400 [] (document (joinView base (humanPlayers game) (Just problem))))
        Right token -> redirect [("Set-Cookie", "trading-session=" <> token <> "; Path=" <> T.encodeUtf8 (base <> "/") <> "; HttpOnly; SameSite=Strict")]
    ("POST", ["orders"]) -> case session of
      Nothing -> respond (htmlResponse status401 [("HX-Redirect", T.encodeUtf8 (base <> "/"))] (H.p "Join as a player first."))
      Just current -> do
        form <- readForm request
        case form >>= parseOrderForm of
          Left problem -> feedback problem
          Right order -> do
            result <- submitWebOrder game current order
            feedback $ case result of
              Right (OrderId oid) -> "Order #" ++ show oid ++ " accepted. Unfilled quantity remains open."
              Left GameClosed -> "The game has settled; orders are closed."
              Left InvalidQuantity -> "Quantity must be a positive integer."
              Left InstrumentDisabled -> "This instrument is disabled for this game."
    ("GET", ["events"]) -> case session of
      Nothing -> respond (htmlResponse status401 [] (H.p "Join as a player first."))
      Just current -> respond (eventStream game current)
    _ -> respond (htmlResponse status404 [] (H.p "Page not found."))

playerSnapshot :: WebGame -> BrowserSession -> IO (Integer, ExchangeState, Maybe Settlement)
playerSnapshot game session = asPlayer game session $ do
  secret <- getMyPrivateNumber
  snapshot <- getExchangeState
  result <- case gamePhase snapshot of
    Trading -> pure Nothing
    Resolved _ -> Just <$> awaitSettlement
  pure (secret, snapshot, result)

-- Subscribe and replay atomically so reconnects cannot miss an update. SSE
-- connections only consume output; disconnecting never stops the player.
eventStream :: WebGame -> BrowserSession -> Response
eventStream game session = responseStream status200
  [(hContentType, "text/event-stream"), (hCacheControl, "no-cache, no-store"), ("X-Accel-Buffering", "no")] $ \send flush -> do
    let player = webPlayer game session
    (updates, initial) <- atomically $ do
      channel <- dupTChan (webUpdates player)
      (snapshot, settled) <- readTVar (webLatest player)
      pure (channel, maybe [] (pure . ExchangeSnapshot) snapshot
                  ++ maybe [] (pure . PlayerSettlement) settled)
    latest <- newTVarIO Nothing
    outputLock <- newMVar ()
    let write chunk = withMVar outputLock (\() -> send chunk >> flush)
        publish info = case info of
          ExchangeSnapshot snapshot -> do
            atomically (writeTVar latest (Just snapshot))
            case gamePhase snapshot of
              Trading -> write (sseHtml "exchange" (exchangeView snapshot Nothing))
              Resolved _ -> pure ()
          PlayerSettlement result -> do
            snapshot <- readTVarIO latest
            forM_ snapshot $ \current -> write (sseHtml "exchange" (exchangeView current (Just result)))
            write "event: closed\ndata: done\n\n"
          _ -> pure ()
        consume [] = do
          info <- atomically (readTChan updates `orElse` playerStopped player)
          consume [info]
        consume (info:rest) = do
          publish info
          case info of
            PlayerSettlement _ -> pure ()
            _ -> consume rest
        heartbeat = forever (threadDelay 15000000 >> write ": keep-alive\n\n")
    runIO $ runConcurrent $ withWorkers [liftIO heartbeat] (liftIO (consume initial))

-- Prefix every line, including user-controlled newlines, per the SSE format.
sseHtml :: BS.ByteString -> Html -> Builder
sseHtml event html = byteString $ "event: " <> event <> "\n"
  <> BS.concat ["data: " <> line <> "\n" | line <- B.split '\n' escaped] <> "\n"
  where
    escaped = B.concatMap (\c -> if c == '\r' then "&#13;" else B.singleton c)
      (LBS.toStrict (renderHtml html))

attr :: H.Tag -> H.AttributeValue -> H.Attribute
attr = customAttribute

panel :: Html -> Html
panel = H.section ! A.class_ "rounded-xl border border-slate-200 bg-white p-6 shadow-sm space-y-4"

inputClass :: H.AttributeValue
inputClass = "w-full rounded border border-slate-300 bg-white p-2 text-slate-900"

document :: Html -> Html
document content = H.docTypeHtml ! A.lang "en" $ do
  H.head $ do
    H.meta ! A.charset "utf-8"
    H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
    H.title "Trading Game · Debug exchange"
    H.script ! A.src "https://cdn.jsdelivr.net/npm/htmx.org@2.0.10/dist/htmx.min.js" $ mempty
    H.script ! A.src "https://cdn.jsdelivr.net/npm/htmx-ext-sse@2.2.4/dist/sse.min.js" $ mempty
    H.script ! A.src "https://cdn.jsdelivr.net/npm/@tailwindcss/browser@4.3.0/dist/index.global.js" $ mempty
  H.body ! A.class_ "min-h-screen bg-slate-100 text-slate-900" $ H.main ! A.class_ "mx-auto max-w-6xl space-y-6 p-6" $ do
    H.header $ do
      H.p ! A.class_ "text-sm font-semibold uppercase tracking-widest text-indigo-600" $ "Local debugging exchange"
      H.h1 ! A.class_ "text-3xl font-bold" $ "Trading Game"
      H.p ! A.class_ "mt-2 text-slate-600" $ "Trade contracts on statistics of the players’ private numbers. Each number is between 1 and 9."
      H.a ! A.href "/" ! A.class_ "text-indigo-700 underline" $ "All games"
    content
    H.footer ! A.class_ "text-sm text-slate-500" $ "In-memory demo · Restart the server to reset."

joinView :: T.Text -> [Player] -> Maybe T.Text -> Html
joinView _ [] _ = panel (H.p "This roster contains only bots; there are no human accounts to join.")
joinView base roster problem = panel $ do
  H.h2 ! A.class_ "text-xl font-semibold" $ "Join as a player"
  H.p "Choose an existing player. Rejoining from any browser returns to the same private number, orders, and account."
  H.p ! A.class_ "text-sm text-slate-600" $ "The roster and private numbers were fixed when this game was created. Every player counts toward the resolutions, even before joining."
  forM_ problem $ \message -> H.p ! A.role "alert" ! A.class_ "text-red-700" $ toHtml message
  H.form ! A.method "post" ! A.action (H.toValue (base <> "/join")) ! A.class_ "max-w-sm space-y-3" $ do
    H.label ! A.for "name" ! A.class_ "block" $ "Player name"
    H.select ! A.id "name" ! A.name "name" ! A.required "" ! A.class_ inputClass $
      forM_ roster $ \player -> H.option ! A.value (H.toValue (displayName player)) $ toHtml (displayName player)
    H.button ! A.type_ "submit" ! A.class_ "rounded bg-indigo-700 px-4 py-2 font-semibold text-white" $ "Join game"

gameView :: T.Text -> BrowserSession -> Integer -> ExchangeState -> Maybe Settlement -> Html
gameView base session secret snapshot settlement = do
  panel $ do
    H.h2 ! A.class_ "text-xl font-semibold" $ toHtml ("Playing as " ++ displayName (sessionPlayer session))
    H.p $ do
      "Your private number: "
      H.strong ! A.class_ "font-mono text-2xl text-indigo-700" $ toHtml (show secret)
  if Set.null (enabledInstruments (gameInfo snapshot)) then panel (H.p "No instruments are enabled for this game.") else panel $ do
    H.h2 ! A.class_ "text-xl font-semibold" $ "New limit order"
    H.form ! A.method "post" ! A.action (H.toValue (base <> "/orders")) ! attr "hx-post" (H.toValue (base <> "/orders")) ! attr "hx-target" "#order-result"
      ! attr "hx-disabled-elt" "find button" ! A.class_ "grid gap-4 sm:grid-cols-5 sm:items-end" $ do
      H.label $ do
        "Instrument"
        H.select ! A.name "instrument" ! A.class_ inputClass $
          forM_ (Set.toAscList (enabledInstruments (gameInfo snapshot))) $ \asset ->
            H.option ! A.value (H.toValue (show asset)) $ toHtml (show asset)
      H.label $ do
        "Side"
        H.select ! A.name "side" ! A.class_ inputClass $ do
          H.option ! A.value "buy" $ "Buy"
          H.option ! A.value "sell" $ "Sell"
      H.label $ do
        "Limit price"
        H.input ! A.name "price" ! A.type_ "text" ! A.required "" ! A.maxlength "100" ! A.placeholder "e.g. 50 or 99/2" ! A.class_ inputClass
      H.label $ do
        "Quantity"
        H.input ! A.name "quantity" ! A.type_ "number" ! A.min "1" ! A.step "1" ! A.value "1" ! A.required "" ! A.class_ inputClass
      H.button ! A.type_ "submit" ! A.class_ "rounded bg-indigo-700 px-4 py-2 font-semibold text-white disabled:opacity-50" $ "Place order"
    H.p ! A.class_ "text-sm text-slate-600" $ "Prices accept integers, decimals, and fractions. A buy is your maximum price; a sell is your minimum."
    H.div ! A.id "order-result" ! attr "aria-live" "polite" $ mempty
  H.div ! attr "hx-ext" "sse" ! attr "sse-connect" (H.toValue (base <> "/events")) ! attr "sse-close" "closed" $
    H.div ! A.id "exchange" ! attr "sse-swap" "exchange" ! A.class_ "space-y-6" $ exchangeView snapshot settlement

exchangeView :: ExchangeState -> Maybe Settlement -> Html
exchangeView snapshot settlement = do
  panel $ do
    H.h2 ! A.class_ "text-xl font-semibold" $ case gamePhase snapshot of
      Trading -> "Exchange open"
      Resolved _ -> "Settled"
    case gamePhase snapshot of
      Trading -> mempty
      Resolved values -> forM_ (Map.toAscList values) $ \(asset, value) ->
        H.p $ toHtml (show asset ++ " = " ++ number value)
    H.p ! A.class_ "text-sm text-slate-600" $ toHtml ("Closes: " ++ show (closesAt (gameInfo snapshot)))
    H.p ! A.class_ "text-sm text-slate-600" $ toHtml ("Last server update: " ++ show (observedAt snapshot))
    forM_ settlement $ \result -> H.p ! A.class_ "font-semibold" $ toHtml ("Your payoff: " ++ number (netPayoff result))
  forM_ settlement $ \result -> panel $ do
    H.h2 ! A.class_ "text-xl font-semibold" $ "Player results"
    table ["Player", "Private number", "Payoff"] $
      forM_ (playerResults result) $ \entry -> H.tr $ do
        cell (displayName (settledPlayer entry))
        cell (show (privateNumber (settledPlayer entry)))
        cell (number (playerPayoff entry))
  panel $ do
    H.h2 ! A.class_ "text-xl font-semibold" $ "Public portfolios"
    H.p ! A.class_ "text-sm text-slate-600" $ "Filled trades only. Open orders are excluded. Negative quantities are short positions."
    H.p ! A.class_ "text-sm text-slate-600" $ "Quantity @ effective price: net spending in this instrument divided by units held, including earlier gains and losses."
    H.div ! A.id "portfolios" $ do
      let assets = Set.toAscList (enabledInstruments (gameInfo snapshot))
      table (["Player"] ++ map (toHtml . show) assets ++ ["Cash"]) $
        forM_ (Map.elems (portfolios snapshot)) $ \portfolio -> H.tr $ do
          cell (portfolioName portfolio)
          forM_ assets $ cell . renderHolding portfolio
          cell (number (portfolioCash portfolio))
  panel $ do
    H.h2 ! A.class_ "text-xl font-semibold" $ "Market reveals"
    H.p "Each event independently selects a player, including bots. Numbers can repeat."
    if null (revealTimes (gameInfo snapshot)) then H.p "No reveals scheduled."
    else H.ol ! A.class_ "space-y-2 font-mono" $
      forM_ (zip (revealTimes (gameInfo snapshot)) (map Just (revealedNumbers snapshot) ++ repeat Nothing)) $ \(time, value) ->
        H.li $ do
          toHtml (show time ++ " · ")
          case value of
            Nothing -> H.span ! A.class_ "text-slate-500" $ "Upcoming"
            Just revealed -> H.strong ! A.class_ "text-indigo-700" $ toHtml (show revealed)
  forM_ (Map.toAscList (orderBook snapshot)) $ \(asset, book) ->
    H.section ! attr "data-instrument" (H.toValue (show asset)) ! A.class_ "space-y-4" $ do
      H.h2 ! A.class_ "text-xl font-semibold" $ toHtml (show asset)
      H.div ! A.class_ "grid gap-6 md:grid-cols-2" $ do
        bookPanel book Buy "Open buys · highest first"
        bookPanel book Sell "Open sells · lowest first"
  panel $ do
    H.h2 ! A.class_ "text-xl font-semibold" $ "Recent trades"
    if null (tradeHistory snapshot) then H.p "No trades yet."
    else table ["Time (UTC)", "Instrument", "Price", "Quantity"] $
      forM_ (take 20 (reverse (tradeHistory snapshot))) $ \trade -> H.tr $ do
        cell (show (tradedAt trade))
        cell (show (tradeInstrument trade))
        cell (priceText (tradePrice trade))
        cell (show (tradeQuantity trade))
  where
    bookPanel book side title = panel $ do
      H.h2 ! A.class_ (if side == Buy then "text-lg font-semibold text-emerald-700" else "text-lg font-semibold text-rose-700") $ title
      let entries = sortOn (\entry -> (if side == Buy then negatePrice (restingPrice entry) else restingPrice entry, restingOrderId entry))
            (filter ((== side) . restingSide) book)
      if null entries then H.p "No open orders."
      else table ["Order", "Price", "Remaining"] $ forM_ entries $ \entry -> H.tr $ do
        let OrderId oid = restingOrderId entry
        cell ("#" ++ show oid)
        cell (priceText (restingPrice entry))
        cell (show (remainingQuantity entry))
    negatePrice (Price value) = Price (negate value)

table :: [Html] -> Html -> Html
table labels rows = H.div ! A.class_ "overflow-x-auto" $ H.table ! A.class_ "w-full text-left text-sm tabular-nums" $ do
  H.thead $ H.tr $ forM_ labels (H.th ! A.class_ "border-b py-2 pr-4 font-semibold")
  H.tbody rows

cell :: String -> Html
cell value = H.td ! A.class_ "border-b border-slate-100 py-2 pr-4 font-mono" $ toHtml value

number :: Rational -> String
number = renderNumber

priceText :: Price -> String
priceText (Price value) = number value

-- Browser tokens live in a separate table for each game, even when names match.
data WebLobby = WebLobby
  { lobbyManager :: GameManager
  , lobbyGames :: MVar (Maybe (Map.Map GameId WebGame))
  , lobbyDuration :: NominalDiffTime
  }

newWebLobby :: GameManager -> NominalDiffTime -> IO WebLobby
newWebLobby manager duration = WebLobby manager <$> newMVar (Just Map.empty) <*> pure duration

withWebLobby :: GameManager -> NominalDiffTime -> (WebLobby -> IO a) -> IO a
withWebLobby manager duration = bracket (newWebLobby manager duration) closeWebLobby

closeWebLobby :: WebLobby -> IO ()
closeWebLobby lobby = do
  games <- modifyMVar (lobbyGames lobby) (\current -> pure (Nothing, maybe [] Map.elems current))
  mapM_ closeWebGame games

humanPlayers :: WebGame -> [Player]
humanPlayers game = filter (\player -> playerID player `notElem` map playerID (webBots game)) (webPlayers game)

managedWebGame :: WebLobby -> GameSummary -> IO (Maybe WebGame)
managedWebGame lobby summary = do
  active <- gameRuntime (lobbyManager lobby) (summaryId summary)
  case active of
    Nothing -> pure Nothing
    Just runtime -> do
      roster <- players <$> readMVar (runtimeEngine runtime)
      let bots = [player | (player, entry) <- zip roster (gameRoster (summaryConfig summary))
                         , rosterType entry /= HumanPlayer]
      modifyMVarMasked (lobbyGames lobby) $ \cached -> case cached of
        Nothing -> ioError (userError "Web lobby is closed")
        Just games -> case Map.lookup (summaryId summary) games of
          Just game -> pure (cached, Just game)
          Nothing -> do
            game <- newWebGameFromRuntime runtime roster bots
            pure (Just (Map.insert (summaryId summary) game games), Just game)

gamePath :: GameId -> T.Text
gamePath (GameId gid) = "/games/" <> T.pack (show gid)

lobbyApplication :: WebLobby -> Application
lobbyApplication lobby request respond = do
  let manager = lobbyManager lobby
      manage :: Eff '[ManageGames, IOE] a -> IO a
      manage action = runIO (runManageGames manager action)
      page status body = respond (htmlResponse status [] (document body))
      notFound = page status404 (H.p "Game not found.")
  case (requestMethod request, pathInfo request) of
    ("GET", []) -> do
      summaries <- manage listAllGames
      now <- getCurrentTime
      let stamp :: UTCTime -> BS.ByteString
          stamp = B.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"
      page status200 $ do
        H.div ! attr "hx-ext" "sse" ! attr "sse-connect" "/games/events" $
          H.div ! attr "sse-swap" "games" $ gamesView summaries
        creationView (lobbyDuration lobby) [("start", stamp now), ("end", stamp (addUTCTime (lobbyDuration lobby) now))] Nothing
    ("GET", ["roster-row"]) -> respond (htmlResponse status200 [] (rosterRow "" "human"))
    ("POST", ["games"]) -> do
      form <- readForm request
      result <- case form >>= parseNewGameForm of
        Left problem -> pure (Left problem)
        Right config -> either (Left . creationError) Right <$> manage (createNewGame config)
      case result of
        Right gid -> respond (htmlResponse status303 [(hLocation, T.encodeUtf8 (gamePath gid <> "/"))] mempty)
        Left problem -> page status400 (creationView (lobbyDuration lobby) (either (const []) id form) (Just problem))
    ("GET", ["games", "events"]) -> respond $ directoryStream manager $ do
      summaries <- manage listAllGames
      pure ("games", gamesView summaries, False)
    (_, "games":rawId:rest) -> case readMaybe (T.unpack rawId) of
      Nothing -> notFound
      Just ident -> do
        found <- manage (lookupGame (GameId ident))
        case found of
          Nothing -> notFound
          Just summary -> do
            let base = gamePath (summaryId summary)
            case (requestMethod request, filter (not . T.null) rest) of
              ("GET", ["status"]) -> respond $ directoryStream manager $ do
                current <- manage (lookupGame (summaryId summary))
                pure ("availability", maybe (H.p "Game not found.") availabilityView current,
                      maybe True ((/= Upcoming) . summaryStatus) current)
              _ -> do
                game <- managedWebGame lobby summary
                case game of
                  Just active -> gameApplication base active
                    request { pathInfo = filter (not . T.null) rest } respond
                  Nothing -> case (requestMethod request, filter (not . T.null) rest) of
                    ("GET", []) -> page status200 $
                      H.div ! attr "hx-ext" "sse" ! attr "sse-connect" (H.toValue (base <> "/status"))
                        ! attr "sse-close" "closed" $
                        H.div ! attr "sse-swap" "availability" $ availabilityView summary
                    _ -> page status409 (H.p "This game is not available to join or trade yet.")
    _ -> notFound

-- An STM revision wakes streams on creation, start, completion, or failure.
-- Read it before rendering so no concurrent update can be lost.
directoryStream :: GameManager -> IO (BS.ByteString, Html, Bool) -> Response
directoryStream manager snapshot = responseStream status200
  [(hContentType, "text/event-stream"), (hCacheControl, "no-cache, no-store"), ("X-Accel-Buffering", "no")] $ \send flush -> do
    lock <- newMVar ()
    let write chunk = withMVar lock (\() -> send chunk >> flush)
        loop = do
          revision <- readTVarIO (gameRevision manager)
          (event, html, finished) <- snapshot
          write (sseHtml event html)
          if finished then write "event: closed\ndata: done\n\n" else do
            atomically (readTVar (gameRevision manager) >>= check . (/= revision))
            loop
        heartbeat = forever (threadDelay 15000000 >> write ": keep-alive\n\n")
    runIO $ runConcurrent $ withWorkers [liftIO heartbeat] (liftIO loop)

gamesView :: [GameSummary] -> Html
gamesView summaries = panel $ do
  H.h2 ! A.class_ "text-xl font-semibold" $ "All games"
  if null summaries then H.p "No games yet. Create the first game below."
  else table ["Game", "Status", "Starts (UTC)", "Ends (UTC)", "Players"] $
    forM_ summaries $ \summary -> H.tr $ do
      let GameId gid = summaryId summary
          config = summaryConfig summary
      H.td ! A.class_ "py-2 pr-4" $ H.a ! A.href (H.toValue (gamePath (summaryId summary) <> "/"))
        ! A.class_ "text-indigo-700 underline" $ toHtml ("Game " ++ show gid)
      cell (statusLabel (summaryStatus summary))
      cell (show (gameStart config))
      cell (show (gameEnd config))
      cell (show (length (gameRoster config)))

statusLabel :: GameStatus -> String
statusLabel Upcoming = "Upcoming"
statusLabel Running = "Running"
statusLabel Completed = "Completed"
statusLabel Failed = "Unavailable"

availabilityView :: GameSummary -> Html
availabilityView summary = do
  panel $ do
    H.h2 ! A.class_ "text-xl font-semibold" $ toHtml (statusLabel (summaryStatus summary))
    H.p $ toHtml ("Starts: " ++ show (gameStart config) ++ " · Ends: " ++ show (gameEnd config))
    H.p "Reveal times (UTC)"
    H.ul $ forM_ (configuredRevealTimes config) (H.li . toHtml . show)
    H.ul $ forM_ (gameRoster config) $ \entry ->
      H.li $ toHtml (rosterName entry ++ " · " ++ playerTypeLabel (rosterType entry))
  case summaryStatus summary of
    Upcoming -> panel (H.p "Joining opens at the start time. This page updates automatically.")
    Failed -> panel (H.p "The game could not run.")
    _ -> joinView (gamePath (summaryId summary))
      [Player (PlayerId n) (rosterName entry) 0 | (n, entry) <- zip [1..] (gameRoster config)
                                            , rosterType entry == HumanPlayer] Nothing
  where config = summaryConfig summary

playerTypeLabel :: PlayerType -> String
playerTypeLabel HumanPlayer = "Human"
playerTypeLabel RandomTradingBot = "Random trading bot"
playerTypeLabel MarketMakingBot = "Market-making bot"

creationError :: CreateGameError -> String
creationError InvalidTimeWindow = "End time must be later than start time."
creationError EmptyRoster = "Add at least one player."
creationError BlankPlayerName = "Every player needs a name."
creationError DuplicatePlayerNames = "Player names must be unique within this game."
creationError InvalidRevealTimes = "Reveal times must be at or after the start and before the end."
creationError ManagerClosed = "The server is shutting down."

-- UTC is explicit: datetime-local values have no browser timezone attached.
parseNewGameForm :: [(BS.ByteString, BS.ByteString)] -> Either String NewGameConfig
parseNewGameForm fields = do
  start <- timestamp "start"
  end <- timestamp "end"
  schedule <- case lookup "reveal-mode" fields of
    Nothing -> pure Nothing
    Just "default" -> pure Nothing
    Just "custom" -> Just <$> traverse parseReveal
      (words (B.unpack (maybe "" id (lookup "reveal-times" fields))))
    _ -> Left "Choose a supported reveal schedule."
  selected <- if lookup "instruments-present" fields == Nothing && not (any ((== "instrument") . fst) fields)
    then pure allInstruments
    else Set.fromList <$> traverse parseInstrument [value | (key, value) <- fields, key == "instrument"]
  names <- traverse decode [value | (key, value) <- fields, key == "player-name"]
  types <- traverse parseType [value | (key, value) <- fields, key == "player-type"]
  if length names /= length types then Left "Every roster row needs a name and player type."
  else pure (NewGameConfig start end [RosterEntry (T.unpack name) kind
        | (name, kind) <- zip names types, not (T.null name)] selected schedule)
  where
    parseInstrument value = case readMaybe (B.unpack value) of
      Just asset -> Right asset
      Nothing -> Left "Choose a supported instrument."
    decode value = either (const (Left "Use valid UTF-8 player names.")) Right (T.decodeUtf8' value)
    timestamp key = case lookup key fields >>= parseTimestamp . B.unpack of
      Nothing -> Left "Enter start and end times in UTC."
      Just value -> Right value
    parseReveal value = maybe (Left "Enter each reveal as a UTC timestamp, e.g. 2026-09-22T12:30:00.") Right (parseTimestamp value)
    parseTimestamp value = case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q" value of
      Just time -> Just time
      Nothing -> parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M" value
    parseType "human" = Right HumanPlayer
    parseType "random" = Right RandomTradingBot
    parseType "maker" = Right MarketMakingBot
    parseType _ = Left "Choose a supported player type."

creationView :: NominalDiffTime -> [(BS.ByteString, BS.ByteString)] -> Maybe String -> Html
creationView duration fields problem = panel $ do
  H.h2 ! A.class_ "text-xl font-semibold" $ "Create a game"
  H.p "Choose a time window in UTC. Past start times begin immediately; an elapsed window settles immediately."
  H.p "Private numbers are drawn by the server. The roster cannot change after creation."
  forM_ problem $ \message -> H.p ! A.role "alert" ! A.class_ "text-red-700" $ toHtml message
  H.form ! A.method "post" ! A.action "/games" ! A.class_ "space-y-4" $ do
    forM_ [("start", "Starts (UTC)"), ("end", "Ends (UTC)")] $ \(key, label) -> H.label ! A.class_ "block" $ do
      toHtml (label :: String)
      H.input ! A.type_ "datetime-local" ! A.name (H.toValue (B.unpack key)) ! A.required "" ! A.step "1"
        ! A.value (H.toValue (B.unpack (maybe "" id (lookup key fields)))) ! A.class_ inputClass
    H.p ! A.class_ "text-sm text-slate-600" $ toHtml ("Suggested duration: " ++ show duration ++ ". All times are UTC.")
    H.fieldset ! A.class_ "space-y-2" $ do
      H.legend ! A.class_ "font-semibold" $ "Market reveals"
      H.label $ do
        "Schedule"
        H.select ! A.name "reveal-mode" ! A.class_ inputClass $
          forM_ [("default", "Default: one reveal per player, evenly spaced"), ("custom", "Custom times (leave empty for no reveals)")] $ \(value, label) ->
            (if lookup "reveal-mode" fields == Just value then (! A.selected "") else id) $
              H.option ! A.value (H.toValue (B.unpack value)) $ toHtml (label :: String)
      H.label ! A.class_ "block" $ do
        "Custom reveal times (UTC), one per line"
        H.textarea ! A.name "reveal-times" ! A.rows "4" ! A.class_ inputClass
          ! A.placeholder "2026-09-22T12:30:00" $
            toHtml (B.unpack (maybe "" id (lookup "reveal-times" fields)))
      H.p ! A.class_ "text-sm text-slate-600" $ "Each line adds one reveal. Times must be within the game, before the end. Repeated times are allowed. The default uses start + (end − start) × i / (N + 1), for i = 1 … N, including bots."
    H.fieldset ! A.class_ "space-y-2" $ do
      H.legend ! A.class_ "font-semibold" $ "Enabled instruments"
      H.input ! A.type_ "hidden" ! A.name "instruments-present" ! A.value "yes"
      forM_ (Set.toAscList allInstruments) $ \asset -> H.label ! A.class_ "mr-4 inline-flex items-center gap-2" $ do
        let selected = lookup "instruments-present" fields == Nothing
              || ("instrument", B.pack (show asset)) `elem` fields
        (if selected then (! A.checked "") else id) $
          H.input ! A.type_ "checkbox" ! A.name "instrument" ! A.value (H.toValue (show asset))
        toHtml (show asset)
      H.p ! A.class_ "text-sm text-slate-600" $ "Each instrument has its own order book. StdDev uses population standard deviation, rounded to six decimal places."
    H.div ! A.id "roster" ! A.class_ "space-y-3" $ do
      H.p "Players (leave unused rows blank)"
      let names = [value | (key, value) <- fields, key == "player-name"]
          types = [value | (key, value) <- fields, key == "player-type"]
          rows = if null names then [("alice", "human"), ("bob", "human"), ("market-maker", "maker"), ("random-trader", "random")]
                 else zip names types
      forM_ (rows ++ replicate 4 ("", "human")) $ \(name, kind) -> rosterRow name kind
    H.button ! A.type_ "button" ! attr "hx-get" "/roster-row" ! attr "hx-target" "#roster" ! attr "hx-swap" "beforeend"
      ! A.class_ "rounded border px-4 py-2" $ "Add player row"
    H.button ! A.type_ "submit" ! A.class_ "rounded bg-indigo-700 px-4 py-2 font-semibold text-white" $ "Create game"

rosterRow :: BS.ByteString -> BS.ByteString -> Html
rosterRow name kind = H.div ! A.class_ "grid gap-3 sm:grid-cols-2" $ do
  H.input ! A.name "player-name" ! A.type_ "text" ! A.placeholder "Player name" ! A.maxlength "100"
    ! attr "aria-label" "Player name" ! A.value (H.toValue (T.decodeUtf8With (\_ _ -> Just '\xfffd') name)) ! A.class_ inputClass
  H.select ! A.name "player-type" ! attr "aria-label" "Player type" ! A.class_ inputClass $
    forM_ [("human", HumanPlayer), ("random", RandomTradingBot), ("maker", MarketMakingBot)] $ \(value, playerType) ->
      (if kind == value then (! A.selected "") else id)
        (H.option ! A.value (H.toValue (B.unpack value))) $ toHtml (playerTypeLabel playerType)

runWebServer :: Int -> NominalDiffTime -> IO ()
runWebServer port duration = do
  clock <- newLiveClock
  withGameManager clock $ \manager -> withWebLobby manager duration $ \lobby -> do
    let settings = Warp.setHost "127.0.0.1" $ Warp.setPort port $ Warp.setBeforeMainLoop
          (putStrLn ("Trading Game: http://127.0.0.1:" ++ show port)) Warp.defaultSettings
    Warp.runSettings settings (lobbyApplication lobby)
