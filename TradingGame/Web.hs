{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- A local debugging adapter. The fixed roster belongs to the host; browser
-- names select existing players and opaque cookies identify browser sessions.
module TradingGame.Web where

import Control.Concurrent.STM
import Control.Effect (Eff, IOE, (:<), liftIO, runIO)
import Control.Monad (forM_, void, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Builder (Builder, byteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isSpace)
import Data.List (find, sortOn)
import qualified Data.Map.Strict as Map
import Data.Ratio (denominator, numerator)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock (NominalDiffTime)
import Network.HTTP.Types
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import Numeric (showHex)
import System.Entropy (getEntropy)
import System.Random (randomRIO)
import Text.Blaze.Html5 (Html, (!), toHtml, customAttribute)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import TradingGame
import TradingGame.Terminal (parseCommand)
import Web.Cookie (parseCookies)

newtype BrowserSession = BrowserSession
  { sessionPlayer :: Player
  }

data WebGame = WebGame
  { webRuntime :: LiveRuntime
  , webRevision :: TVar Integer
  , webSessions :: TVar (Map.Map BS.ByteString BrowserSession)
  , webPlayers :: [Player]
  , webBots :: [Player]
  }

-- Player identities are fixed; only their private numbers are drawn at startup.
webPlayerNames :: [String]
webPlayerNames =
  ["alice", "bob", "carol", "dan", "eve", "fred", "gwen", "hal", "market-maker", "noise-trader"]

newWebGame :: NominalDiffTime -> IO WebGame
newWebGame duration = do
  clock <- newLiveClock
  start <- clockNow clock
  roster <- sequence
    [Player (PlayerId n) name <$> randomRIO (1, 9) | (n, name) <- zip [1..] webPlayerNames]
  revision <- newTVarIO 0
  let changed = atomically (modifyTVar' revision (+1))
      trace (RequestHandled _ _ (SubmitOrder _) _) = changed
      trace (ExchangeClosed _) = changed
      trace _ = pure ()
  runtime <- newLiveRuntime clock trace (newEngine start duration roster)
  sessions <- newTVarIO Map.empty
  pure (WebGame runtime revision sessions roster (drop 8 roster))

-- Each HTTP request reinstalls the existing session and trading interpreters.
-- Identity always comes from the server's cookie table, never form fields.
asPlayer :: WebGame -> BrowserSession -> Eff '[TradingGame, PlayerSession, IOE] a -> IO a
asPlayer game session action = do
  result <- runIO $ runPlayerSession (webPlayers game) $ do
    void (joinGameAsPlayer (displayName (sessionPlayer session)))
    runWithCurrentPlayer (webRuntime game) action
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
joinWebPlayer game name = case find ((== T.unpack name) . displayName) (webPlayers game) of
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
  values <- traverse field ["side", "price", "quantity"]
  case parseCommand (unwords values) of
    Right (PlaceOrder order) -> Right order
    _ -> Left "Choose buy or sell, a numeric price, and a positive integer quantity."
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
webApplication game request respond = do
  session <- lookupSession game request
  let page content = respond (htmlResponse status200 [] (document content))
      feedback message = respond (htmlResponse status200 [] (H.p ! A.role "status" $ toHtml message))
      redirect headers = respond (htmlResponse status303 ((hLocation, "/"):headers) mempty)
  case (requestMethod request, pathInfo request) of
    ("GET", []) -> case session of
      Nothing -> page (joinView (webPlayers game) Nothing)
      Just current -> do
        (snapshot, settlement) <- playerSnapshot game current
        page (gameView current snapshot settlement)
    ("POST", ["join"]) -> do
      form <- readForm request
      result <- case form of
        Left problem -> pure (Left (T.pack problem))
        Right fields -> case lookup "name" fields >>= either (const Nothing) Just . T.decodeUtf8' of
          Nothing -> pure (Left "Enter a valid player name.")
          Just name -> joinWebPlayer game name
      case result of
        Left problem -> respond (htmlResponse status400 [] (document (joinView (webPlayers game) (Just problem))))
        Right token -> redirect [("Set-Cookie", "trading-session=" <> token <> "; Path=/; HttpOnly; SameSite=Strict")]
    ("POST", ["orders"]) -> case session of
      Nothing -> respond (htmlResponse status401 [("HX-Redirect", "/")] (H.p "Join as a player first."))
      Just current -> do
        form <- readForm request
        case form >>= parseOrderForm of
          Left problem -> feedback problem
          Right order -> do
            result <- asPlayer game current (submitOrder order)
            feedback $ case result of
              Right (OrderId oid) -> "Order #" ++ show oid ++ " accepted. Unfilled quantity remains open."
              Left GameClosed -> "The game has settled; orders are closed."
              Left InvalidQuantity -> "Quantity must be a positive integer."
    ("GET", ["events"]) -> case session of
      Nothing -> respond (htmlResponse status401 [] (H.p "Join as a player first."))
      Just current -> respond (eventStream game current)
    _ -> respond (htmlResponse status404 [] (H.p "Page not found."))

playerSnapshot :: WebGame -> BrowserSession -> IO (ExchangeState, Maybe Settlement)
playerSnapshot game session = asPlayer game session $ do
  snapshot <- getExchangeState
  result <- case gamePhase snapshot of
    Trading -> pure Nothing
    Resolved _ -> Just <$> awaitSettlement
  pure (snapshot, result)

-- A revision counter coalesces updates for slow clients without an unbounded
-- event queue. Read the revision BEFORE the snapshot: a concurrent change can
-- cause a harmless duplicate, but cannot be missed. Reconnects get a full view.
eventStream :: WebGame -> BrowserSession -> Response
eventStream game session = responseStream status200
  [(hContentType, "text/event-stream"), (hCacheControl, "no-cache, no-store"), ("X-Accel-Buffering", "no")] $ \send flush -> do
    let publish = do
          revision <- readTVarIO (webRevision game)
          (snapshot, settlement) <- playerSnapshot game session
          send (sseHtml "exchange" (exchangeView snapshot settlement))
          flush
          case gamePhase snapshot of
            Resolved _ -> send "event: closed\ndata: done\n\n" >> flush
            Trading -> listen revision
        listen previous = do
          heartbeat <- registerDelay 15000000
          changed <- atomically $
            (readTVar (webRevision game) >>= \current -> check (current /= previous) >> pure True)
            `orElse` (readTVar heartbeat >>= check >> pure False)
          if changed then publish else send ": keep-alive\n\n" >> flush >> listen previous
    publish

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
      H.p ! A.class_ "mt-2 text-slate-600" $ "Trade contracts on the sum of ten private numbers. Each number is between 1 and 9."
    content
    H.footer ! A.class_ "text-sm text-slate-500" $ "In-memory demo · 8 human players + 2 trading bots · Restart the server to reset."

joinView :: [Player] -> Maybe T.Text -> Html
joinView roster problem = panel $ do
  H.h2 ! A.class_ "text-xl font-semibold" $ "Join as a player"
  H.p "Choose an existing player. Rejoining from any browser returns to the same private number, orders, and account."
  H.p ! A.class_ "text-sm text-slate-600" $ "All ten private numbers are fixed at server startup; every player counts toward the sum, even before joining. The clock starts when the server starts."
  forM_ problem $ \message -> H.p ! A.role "alert" ! A.class_ "text-red-700" $ toHtml message
  H.form ! A.method "post" ! A.action "/join" ! A.class_ "max-w-sm space-y-3" $ do
    H.label ! A.for "name" ! A.class_ "block" $ "Player name"
    H.select ! A.id "name" ! A.name "name" ! A.required "" ! A.class_ inputClass $
      forM_ roster $ \player -> H.option ! A.value (H.toValue (displayName player)) $ toHtml (displayName player)
    H.button ! A.type_ "submit" ! A.class_ "rounded bg-indigo-700 px-4 py-2 font-semibold text-white" $ "Join game"

gameView :: BrowserSession -> ExchangeState -> Maybe Settlement -> Html
gameView session snapshot settlement = do
  panel $ do
    H.h2 ! A.class_ "text-xl font-semibold" $ toHtml ("Playing as " ++ displayName (sessionPlayer session))
    H.p $ do
      "Your private number: "
      H.strong ! A.class_ "font-mono text-2xl text-indigo-700" $ toHtml (show (privateNumber (sessionPlayer session)))
  panel $ do
    H.h2 ! A.class_ "text-xl font-semibold" $ "New limit order"
    H.form ! A.method "post" ! A.action "/orders" ! attr "hx-post" "/orders" ! attr "hx-target" "#order-result"
      ! attr "hx-disabled-elt" "find button" ! A.class_ "grid gap-4 sm:grid-cols-4 sm:items-end" $ do
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
  H.div ! attr "hx-ext" "sse" ! attr "sse-connect" "/events" ! attr "sse-close" "closed" $
    H.div ! A.id "exchange" ! attr "sse-swap" "exchange" ! A.class_ "space-y-6" $ exchangeView snapshot settlement

exchangeView :: ExchangeState -> Maybe Settlement -> Html
exchangeView snapshot settlement = do
  panel $ do
    H.h2 ! A.class_ "text-xl font-semibold" $ case gamePhase snapshot of
      Trading -> "Exchange open"
      Resolved total -> toHtml ("Settled · sum = " ++ show total)
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
  H.div ! A.class_ "grid gap-6 md:grid-cols-2" $ do
    bookPanel Buy "Open buys · highest first"
    bookPanel Sell "Open sells · lowest first"
  panel $ do
    H.h2 ! A.class_ "text-xl font-semibold" $ "Recent trades"
    if null (tradeHistory snapshot) then H.p "No trades yet. The bots trade every few seconds."
    else table ["Time (UTC)", "Price", "Quantity"] $
      forM_ (take 20 (reverse (tradeHistory snapshot))) $ \trade -> H.tr $ do
        cell (show (tradedAt trade))
        cell (priceText (tradePrice trade))
        cell (show (tradeQuantity trade))
  where
    bookPanel side title = panel $ do
      H.h2 ! A.class_ (if side == Buy then "text-lg font-semibold text-emerald-700" else "text-lg font-semibold text-rose-700") $ title
      let entries = sortOn (\entry -> (if side == Buy then negatePrice (restingPrice entry) else restingPrice entry, restingOrderId entry))
            (filter ((== side) . restingSide) (orderBook snapshot))
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
number value | denominator value == 1 = show (numerator value)
             | otherwise = show (numerator value) ++ "/" ++ show (denominator value)

priceText :: Price -> String
priceText (Price value) = number value

-- Both bots use only TradingGame effects. One maintains a small two-sided
-- book; the other crosses its best quotes, so a fresh demo visibly trades.
marketMaker :: TradingGame :< effs => Eff effs ()
marketMaker = do
  secret <- getMyPrivateNumber
  let bid = Price (fromInteger (secret + 9 * 5 - 2))
      ask = Price (fromInteger (secret + 9 * 5 + 2))
      loop = do
        snapshot <- getExchangeState
        when (gamePhase snapshot == Trading) $ do
          when (not (any ((== Buy) . restingSide) (orderBook snapshot))) $ void (submitOrder (LimitOrder Buy bid 5))
          when (not (any ((== Sell) . restingSide) (orderBook snapshot))) $ void (submitOrder (LimitOrder Sell ask 5))
          wait 2
          loop
  loop

noiseTrader :: TradingGame :< effs => Eff effs ()
noiseTrader = loop Buy
  where
    loop side = do
      wait 3
      snapshot <- getExchangeState
      when (gamePhase snapshot == Trading) $ do
        let opposite = filter ((/= side) . restingSide) (orderBook snapshot)
            quotes = sortOn restingPrice opposite
            best = if side == Buy then quotes else reverse quotes
        forM_ (take 1 best) $ \entry -> void (submitOrder (LimitOrder side (restingPrice entry) 1))
        loop (if side == Buy then Sell else Buy)

runWebServer :: Int -> NominalDiffTime -> IO ()
runWebServer port duration = do
  game <- newWebGame duration
  let botActions = zipWith (\player program -> liftIO (asPlayer game (BrowserSession player) program))
        (webBots game) [marketMaker, noiseTrader]
      settings = Warp.setHost "127.0.0.1" $ Warp.setPort port $ Warp.setBeforeMainLoop
        (putStrLn ("Trading Game: http://127.0.0.1:" ++ show port ++ " (" ++ show duration ++ ", 8 human players + 2 bots)")) Warp.defaultSettings
  runIO $ runConcurrent $ withWorkers
    (liftIO (void (runExchange (webRuntime game))) : botActions)
    (liftIO (Warp.runSettings settings (webApplication game)))
  
