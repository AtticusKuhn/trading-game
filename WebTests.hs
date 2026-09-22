{-# LANGUAGE OverloadedStrings #-}

module WebTests (webProperties) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.MVar (readMVar)
import Control.Concurrent.STM (readTVarIO)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Either (isRight)
import Data.List (nub)
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
  Right token <- claimSeat game "alice"
  Right _ <- claimSeat game "bob"
  initial <- readMVar (runtimeEngine (webRuntime game))
  let fields = ("player", "seat-2") : orderFields order
      headers = [(hCookie, "trading-session=" <> token)]
      (expected, _) = handleRequest (opensAt (engineInfo initial)) (playerID (head (webSeats game))) (SubmitOrder order) initial
  response <- runSession (srequest (post "/orders" headers fields)) (webApplication game)
  actual <- readMVar (runtimeEngine (webRuntime game))
  pure $ conjoin [simpleStatus response === status200, engineBook actual === engineBook expected]

-- Concurrent claims never share a seat or change the game's fixed secrets.
prop_claims :: Property
prop_claims = forAll (listOf1 (elements ["alice", "bob", "carol", "dan", "eve", "fred", "gwen", "hal", "ian", "jane"])) $ \names -> ioProperty $ do
  game <- newWebGame 3600
  before <- readMVar (runtimeEngine (webRuntime game))
  outcomes <- mapConcurrently (claimSeat game) names
  sessions <- Map.elems <$> readTVarIO (webSessions game)
  after <- readMVar (runtimeEngine (webRuntime game))
  let ids = map (playerID . sessionPlayer) sessions
      expected = min 8 (length (nub names))
  pure $ conjoin [length (filter isRight outcomes) === expected, length (nub ids) === expected, after === before]

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

-- Names pass through the real join route and HTML escaping, including Unicode.
prop_nameRoundTrip :: Property
prop_nameRoundTrip = forAll (listOf1 (elements ['a', 'Z', 'é', '界', '<', '>', '&', '"'])) $ \chars -> ioProperty $ do
  game <- newWebGame 3600
  let name = T.pack (take 40 chars)
  joined <- runSession (srequest (post "/join" [] [("name", T.encodeUtf8 name)])) (webApplication game)
  let cookie = maybe "" (B.takeWhile (/= ';')) (lookup "Set-Cookie" (simpleHeaders joined))
      pageRequest = defaultRequest { requestHeaders = [(hCookie, cookie)] }
  selected <- lookupSession game pageRequest
  page <- runSession (request pageRequest) (webApplication game)
  pure $ conjoin [simpleStatus joined === status303, fmap sessionName selected === Just name,
    property (LBS.toStrict (renderHtml (toHtml name)) `B.isInfixOf` LBS.toStrict (simpleBody page))]

webProperties :: [Property]
webProperties = [prop_requiresSession, prop_boundOrder, prop_claims, property prop_sseFraming, prop_nameRoundTrip]
