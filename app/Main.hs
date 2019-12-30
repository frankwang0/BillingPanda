module Main where

import ClassyPrelude
import qualified Adapter.InMemory.Auth as M
import qualified Adapter.PostgreSQL.Auth as PG
import Domain.Auth
import Control.Monad.Reader
import Control.Monad.Fail
import qualified Control.Monad.Catch as E
import qualified Adapter.Redis.Auth as Redis
import qualified Adapter.RabbitMQ.Common as MQ
import qualified Adapter.RabbitMQ.Auth as MQAuth
import Text.StringRandom
import qualified Adapter.HTTP.Main as HTTP

type AppState = (PG.State, Redis.State, MQ.State, TVar M.State)
newtype App a = App
    { unApp :: ReaderT AppState IO a
    } deriving (Applicative, Functor, Monad, MonadReader AppState, MonadIO, MonadFail, E.MonadThrow, MonadUnliftIO)

run :: AppState -> App a -> IO a
run state = flip runReaderT state . unApp
-- run state app = flip runReaderT state $ unApp app
-- run state app = runReaderT (unApp app) state

instance AuthRepo App where
    addAuth = PG.addAuth
    setEmailAsVerified = PG.setEmailAsVerified
    findUserByAuth = PG.findUserByAuth
    findEmailFromUserId = PG.findEmailFromUserId

instance EmailNotification App where
    sendVerificationEmail = M.sendVerificationEmail

instance SessionRepo App where
    newSession = Redis.newSession
    findUserIdBySessionId = Redis.findUserIdBySessionId

withState :: (Int -> AppState -> IO ()) -> IO ()
withState action = do
    memoryState <- newTVarIO M.initialState
    PG.withState postgresCfg $ \postgresState ->
      Redis.withState redisCfg $ \redisState ->
        MQ.withState rabbitCfg 16 $ \ rabbitState ->
            action port (postgresState, redisState, rabbitState, memoryState)
    where
      rabbitCfg = "amqp://guest:guest@localhost:5672/%2F"
      redisCfg = "redis://localhost:6379/0"
      port = 3000
      postgresCfg = PG.Config
              { PG.configUrl = "postgresql://localhost/hauth"
              , PG.configStripeCount = 2
              , PG.configMaxOpenConnPerStripe = 5
              , PG.configIdleConnTimeout = 10
              }

main :: IO ()
main =
    withState $ \ port state@(_,_,rabbitState,_) -> do
        let runner = run state
        MQAuth.init rabbitState runner
        -- runner action
        HTTP.main port runner

action :: App ()
action = do
    randEmail <- liftIO $ stringRandomIO "[a-z0-9]{5}@gmail\\.com"
    let email = either undefined id $ mkEmail randEmail
        passw = either undefined id $ mkPassword "Password1"
        auth = Auth email passw
    register auth
    verificationCode <- pollCode email
    verifyEmail verificationCode
    Right session <- login auth
    Just uId <- resolveSessionId session
    Just registeredEmail<- getUser uId
    print (session, uId, registeredEmail)
    where
        pollCode email = do
            result <- M.getVerificationCode email
            case result of
                Nothing -> pollCode email
                Just vCode -> return vCode             