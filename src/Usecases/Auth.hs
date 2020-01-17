module Usecases.Auth where

import Domain.Auth
import Ports.Auth
import ClassyPrelude
import Control.Monad.Except
import Control.Monad.Fail
import qualified Control.Monad.Catch as E
import qualified Adapters.Redis.Auth as Redis
import qualified Adapters.RabbitMQ.Common as MQ
import qualified Adapters.RabbitMQ.Auth as MQAuth
import qualified Adapters.InMemory.Auth as M
import qualified Adapters.PostgreSQL.Auth as PG

type AppState = (PG.State, Redis.State, MQ.State, TVar M.State)
newtype App a = App
    { unApp :: ReaderT AppState IO a
    } deriving (Applicative, Functor, Monad, MonadReader AppState, MonadIO, MonadFail, E.MonadThrow, MonadUnliftIO)

run :: AppState -> App a -> IO a
run state = flip runReaderT state . unApp
-- run state app = flip runReaderT state $ unApp app
-- run state app = runReaderT (unApp app) state

instance UserRepo App where
    addUser = PG.addUser
    setEmailAsVerified = PG.setEmailAsVerified
    authenticate = PG.authenticate
    findEmailByUserId = PG.findEmailByUserId

instance EmailNotification App where
    sendVerificationEmail = M.sendVerificationEmail

instance UserSessionRepo App where
    newSession = Redis.newSession
    findUserIdBySessionId = Redis.findUserIdBySessionId

register :: (UserRepo m, EmailNotification m) => User -> m (Either RegistrationError ())
register user = runExceptT $ do
  (uId, vCode) <- ExceptT $ addUser user
  let email = authEmail user
  lift $ sendVerificationEmail email vCode

verifyEmail :: (UserRepo m, UserSessionRepo m)
            => VerificationCode -> m (Either EmailVerificationError (UserId, Email))
verifyEmail = setEmailAsVerified

login :: (UserRepo m, UserSessionRepo m) => User -> m (Either LoginError SessionId)
login user = runExceptT $ do
  result <- lift $ authenticate user
  case result of
    Nothing -> throwError LoginErrorInvalidAuth
    Just (_, False) -> throwError LoginErrorEmailNotVerified
    Just (uId, _) -> lift $ newSession uId

resolveSessionId :: (UserSessionRepo m) => SessionId -> m (Maybe UserId)
resolveSessionId = findUserIdBySessionId

getUserEmail :: (UserRepo m) => UserId -> m (Maybe Email)
getUserEmail = findEmailByUserId

withState :: (Int -> AppState -> IO ()) -> IO ()
withState action = do
    memoryState <- newTVarIO M.initialState
    PG.withState postgresCfg $ \postgresState ->
      Redis.withState redisCfg $ \redisState ->
        MQ.withState rabbitCfg $ \ rabbitState ->
            action port (postgresState, redisState, rabbitState, memoryState)
    where
      rabbitCfg = MQ.Config
            { MQ.configUrl = "amqp://guest:guest@localhost:5672/%2F"
            , MQ.prefetchCount = 16}
      redisCfg = "redis://localhost:6379/0"
      port = 3000
      postgresCfg = PG.Config
              { PG.configUrl = "postgresql://localhost/hauth"
              , PG.configStripeCount = 2
              , PG.configMaxOpenConnPerStripe = 5
              , PG.configIdleConnTimeout = 10
              }