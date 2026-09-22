{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module SessionTests (sessionProperties) where

import qualified Control.Concurrent.Async as Async
import Control.Concurrent.MVar
import Control.Effect (liftIO, run, runIO)
import Control.Monad (void)
import Data.List (foldl')
import Data.Time.Clock (addUTCTime)
import Test.QuickCheck
import LiveTests (liveProperty, manualClock)
import TestSupport (genEngine, genOrder, genRoster)
import TradingGame

sessionProperties :: [Property]
sessionProperties =
  [ prop_joinIdentity
  , prop_logoutInverse
  , prop_unknownName
  , prop_sessionIsolation
  , prop_rejoinOrders
  , prop_requiresLogin
  , prop_runtimeMembership
  , property prop_waitsAndSettlement
  ]

prop_joinIdentity :: Property
prop_joinIdentity = forAll genRoster $ \roster -> forAll (elements roster) $ \player ->
  let observe = (,) <$> joinGameAsPlayer (displayName player) <*> getCurrentPlayer
      session = run . runPlayerSession roster
  in conjoin
    [ session observe === (Right (playerID player), Just (playerID player))
    , session (joinGameAsPlayer (displayName player) >> observe) === session observe
    ]

prop_logoutInverse :: Property
prop_logoutInverse = forAll genRoster $ \roster -> forAll (elements roster) $ \player ->
  let result = run $ runPlayerSession roster $ do
        void (joinGameAsPlayer (displayName player))
        (,) <$> logout <*> getCurrentPlayer
  in result === (Right (), Nothing)

prop_unknownName :: Property
prop_unknownName = forAll genRoster $ \roster -> forAll (elements roster) $ \player ->
  let missing = replicate (1 + maximum (map (length . displayName) roster)) '!'
      result = run $ runPlayerSession roster $ do
        void (joinGameAsPlayer (displayName player))
        (,) <$> joinGameAsPlayer missing <*> getCurrentPlayer
  in result === (Left (UnknownPlayerName missing), Just (playerID player))

-- Separate connections never inherit another connection's selected identity.
prop_sessionIsolation :: Property
prop_sessionIsolation = forAll genRoster $ \roster -> forAll (elements roster) $ \player ->
  liveProperty "independent sessions" $ do
    result <- runIO $ runPlayerSession roster $ do
      void (joinGameAsPlayer (displayName player))
      other <- liftIO $ runIO $ runPlayerSession roster getCurrentPlayer
      current <- getCurrentPlayer
      pure (other, current)
    pure (result === (Nothing, Just (playerID player)))

-- Inserting logout/rejoin between arbitrary orders is observationally neutral.
-- The pure engine supplies the oracle, including ownership, cash and priority.
prop_rejoinOrders :: Property
prop_rejoinOrders = forAll genEngine $ \initial ->
  forAll (listOf ((,) <$> elements (players initial) <*> genOrder)) $ \orders ->
    liveProperty "rejoining preserves accounts and roster" $ do
      (clock, _) <- manualClock simulationStart
      runtime <- newLiveRuntime clock (const (pure ())) initial
      replies <- runIO $ runPlayerSession (players initial) $ mapM (\(player, order) -> do
        void (joinGameAsPlayer (displayName player))
        result <- runWithCurrentPlayer runtime ((,) <$> getMyPrivateNumber <*> submitOrder order)
        void logout
        pure result) orders
      final <- readMVar (runtimeEngine runtime)
      let expected = foldl' (\engine (player, order) ->
            fst (handleRequest simulationStart (playerID player) (SubmitOrder order) engine)) initial orders
      pure $ conjoin
        [ final === expected
        , players final === players initial
        , replies === [Right (privateNumber player, Right (OrderId oid)) |
            ((player, _), oid) <- zip orders [nextOrderId (engineBook initial)..]]
        ]

-- A rejected session cannot even run unrelated IO in the inner computation.
prop_requiresLogin :: Property
prop_requiresLogin = forAll genEngine $ \initial -> forAll genOrder $ \order ->
  liveProperty "logged-out computation is not executed" $ do
    (clock, _) <- manualClock simulationStart
    runtime <- newLiveRuntime clock (const (pure ())) initial
    result <- runIO $ runPlayerSession (players initial) $
      runWithCurrentPlayer runtime (liftIO (fail "entered without login") >> submitOrder order)
    final <- readMVar (runtimeEngine runtime)
    pure $ conjoin [result === Left NotLoggedIn, final === initial]

prop_runtimeMembership :: Property
prop_runtimeMembership = forAll genEngine $ \initial ->
  liveProperty "session identity must belong to this game" $ do
    (clock, _) <- manualClock simulationStart
    runtime <- newLiveRuntime clock (const (pure ())) initial
    let outsider = Player (PlayerId (1 + maximum [n | PlayerId n <- map playerID (players initial)])) "outsider" 0
    result <- runIO $ runPlayerSession [outsider] $ do
      void (joinGameAsPlayer (displayName outsider))
      runWithCurrentPlayer runtime getMyPrivateNumber
    pure (result === Left (UnknownPlayerId (playerID outsider)))

-- Manual time and a request barrier exercise both suspended interpreter paths.
prop_waitsAndSettlement :: Positive Integer -> Integer -> Property
prop_waitsAndSettlement (Positive delay) secret = liveProperty "session waits and resolves" $ do
  (clock, advance) <- manualClock simulationStart
  requested <- newEmptyMVar :: IO (MVar ())
  let player = Player (PlayerId 1) "caller" secret
      close = addUTCTime (fromInteger (delay + 1)) simulationStart
      trace event = case event of
        RequestHandled _ _ (Wait _) _ -> putMVar requested ()
        RequestHandled _ _ AwaitSettlement _ -> putMVar requested ()
        _ -> pure ()
  runtime <- newLiveRuntime clock trace (newEngine simulationStart (fromInteger (delay + 1)) [player])
  let action = runIO $ runPlayerSession [player] $ do
        void (joinGameAsPlayer (displayName player))
        runWithCurrentPlayer runtime (wait (fromInteger delay) >> awaitSettlement)
  Async.withAsync action $ \caller -> do
    takeMVar requested
    advance (addUTCTime (fromInteger delay) simulationStart)
    takeMVar requested
    advance close
    result <- Async.wait caller
    pure (result === Right (Settlement secret 0 [PlayerResult player 0]))
