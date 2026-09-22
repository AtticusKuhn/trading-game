{-# LANGUAGE OverloadedStrings #-}

module WebTests (webProperties) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.MVar (readMVar)
import Control.Concurrent.STM (readTVarIO)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Either (isRight)
import Data.List (find, nub, sort)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Test
import Test.QuickCheck
import TestSupport (genOrder)
import Text.Blaze.Html5 (toHtml)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import TradingGame
import TradingGame.Web

post :: B.ByteString -> RequestHeaders -> [(B.ByteString, B.ByteString)] -> SRequest
post path headers fields = SRequest
  ((setPath defaultRequest path) { requestMethod = "POST", requestHeaders = headers })
  (LBS.fromStrict (renderSimpleQuery False fields))

orderFields :: LimitOrder -> [(B.ByteString, B.ByteString)]
orderFields order =
  [("side", if orderSide order == Buy then "buy" else "sell")
  ,("price", B.pack (priceText (limitPrice order)))
  ,("quantity", B.pack (show (orderQuantity order)))]

-- Arbitrary valid orders cannot change any exchange state without a session.
prop_requiresSession :: Property
prop_requiresSession = forAll genOrder $ \order -> ioProperty $ do
  game <- newWebGame 3600
  before <- readMVar (runtimeEngine (webRuntime game))
  response <- runSession (srequest (post "/orders" [] (orderFields order))) (webApplication game)
  after <- readMVar (runtimeEngine (webRuntime game))
  pure $ conjoin [simpleStatus response === status401, after === before]

-- HTTP parsing and session binding agree with the pure engine for arbitrary
-- orders, even with a forged player ID in the form. Another account is untouched.
prop_boundOrder :: Property
prop_boundOrder = forAll genOrder $ \order -> ioProperty $ do
  game <- newWebGame 3600
  Right token <- joinWebPlayer game "alice"
  Right _ <- joinWebPlayer game "bob"
  initial <- readMVar (runtimeEngine (webRuntime game))
  let fields = ("player", "bob") : orderFields order
      headers = [(hCookie, "trading-session=" <> token)]
      (expected, _) = handleRequest (opensAt (engineInfo initial)) (playerID (head (webPlayers game))) (SubmitOrder order) initial
  response <- runSession (srequest (post "/orders" headers fields)) (webApplication game)
  actual <- readMVar (runtimeEngine (webRuntime game))
  pure $ conjoin [simpleStatus response === status200, engineBook actual === engineBook expected]

-- Concurrent joins are roster lookup: repetitions all succeed with the same
-- identity, unknown names fail, and the complete engine stays unchanged.
prop_joins :: Property
prop_joins = forAllShrink (listOf1 (elements (map T.pack webPlayerNames ++ ["unknown", "Alice", " alice "])))
  (shrinkList (const [])) $ \names -> ioProperty $ do
    game <- newWebGame 3600
    before <- readMVar (runtimeEngine (webRuntime game))
    outcomes <- mapConcurrently (joinWebPlayer game) names
    sessions <- readTVarIO (webSessions game)
    after <- readMVar (runtimeEngine (webRuntime game))
    let expected name = find ((== T.unpack name) . displayName) (players before)
        selected outcome = either (const Nothing) (fmap sessionPlayer . (`Map.lookup` sessions)) outcome
    pure $ conjoin
      [ map selected outcomes === map expected names
      , map isRight outcomes === map (maybe False (const True) . expected) names
      , sort (nub (map (playerID . sessionPlayer) (Map.elems sessions)))
          === sort (nub [playerID player | name <- names, Just player <- [expected name]])
      , after === before
      ]

-- User text cannot inject SSE fields, including through CR and LF line endings.
prop_sseFraming :: String -> Property
prop_sseFraming value =
  let wire = LBS.toStrict (toLazyByteString (sseHtml "exchange" (toHtml value)))
      lines' = B.split '\n' wire
  in conjoin
    [ head lines' === "event: exchange"
    , property (all (\line -> B.null line || "data: " `B.isPrefixOf` line) (tail lines'))
    , property (not (B.elem '\r' wire))
    , property ("\n\n" `B.isSuffixOf` wire)
    ]

-- Rejoining, with or without the original cookie, preserves an account after
-- an arbitrary order; the cookie always selects the named existing player.
prop_rejoin :: Property
prop_rejoin = forAll (elements webPlayerNames) $ \name ->
  forAll genOrder $ \order -> forAll arbitrary $ \sameBrowser -> ioProperty $ do
    game <- newWebGame 3600
    Right token <- joinWebPlayer game (T.pack name)
    let headers = [(hCookie, "trading-session=" <> token)]
    _ <- runSession (srequest (post "/orders" headers (orderFields order))) (webApplication game)
    before <- readMVar (runtimeEngine (webRuntime game))
    joined <- runSession (srequest (post "/join" (if sameBrowser then headers else [])
      [("name", T.encodeUtf8 (T.pack name))])) (webApplication game)
    let cookie = maybe "" (B.takeWhile (/= ';')) (lookup "Set-Cookie" (simpleHeaders joined))
        pageRequest = defaultRequest { requestHeaders = [(hCookie, cookie)] }
    selected <- lookupSession game pageRequest
    page <- runSession (request pageRequest) (webApplication game)
    after <- readMVar (runtimeEngine (webRuntime game))
    pure $ conjoin
      [ simpleStatus joined === status303
      , fmap sessionPlayer selected === find ((== name) . displayName) (players before)
      , property (LBS.toStrict (renderHtml (toHtml ("Playing as " ++ name))) `B.isInfixOf` LBS.toStrict (simpleBody page))
      , after === before
      ]

-- Unknown names are errors even for an already joined browser, and cannot
-- change either the roster/account state or the browser's existing sessions.
prop_unknownPlayer :: Property
prop_unknownPlayer = forAllShrink (arbitrary `suchThat` (`notElem` webPlayerNames))
  (filter (`notElem` webPlayerNames) . shrink) $ \name ->
  forAll arbitrary $ \loggedIn -> ioProperty $ do
    game <- newWebGame 3600
    Right token <- joinWebPlayer game "alice"
    before <- readMVar (runtimeEngine (webRuntime game))
    sessions <- readTVarIO (webSessions game)
    let headers = if loggedIn then [(hCookie, "trading-session=" <> token)] else []
    response <- runSession (srequest (post "/join" headers [("name", T.encodeUtf8 (T.pack name))])) (webApplication game)
    after <- readMVar (runtimeEngine (webRuntime game))
    remaining <- readTVarIO (webSessions game)
    pure $ conjoin
      [ simpleStatus response === status400
      , lookup "Set-Cookie" (simpleHeaders response) === Nothing
      , fmap sessionPlayer remaining === fmap sessionPlayer sessions
      , after === before
      ]

webProperties :: [Property]
webProperties = [prop_requiresSession, prop_boundOrder, prop_joins, property prop_sseFraming, prop_rejoin, prop_unknownPlayer]
