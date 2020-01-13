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

instance AuthRepo App where
    addAuth = PG.addAuth
    setEmailAsVerified = PG.setEmailAsVerified
    findUserByAuth = PG.findUserByAuth
    findEmailByUserId = PG.findEmailByUserId

instance EmailNotification App where
    sendVerificationEmail = M.sendVerificationEmail

instance SessionRepo App where
    newSession = Redis.newSession
    findUserIdBySessionId = Redis.findUserIdBySessionId

register :: (AuthRepo m, EmailNotification m) => Auth -> m (Either RegistrationError ())
register auth = runExceptT $ do
  (uId, vCode) <- ExceptT $ addAuth auth
  let email = authEmail auth
  lift $ sendVerificationEmail email vCode

verifyEmail :: (AuthRepo m, SessionRepo m) 
            => VerificationCode -> m (Either EmailVerificationError (UserId, Email))
verifyEmail = setEmailAsVerified

login :: (AuthRepo m, SessionRepo m) => Auth -> m (Either LoginError SessionId)
login auth = runExceptT $ do
  result <- lift $ findUserByAuth auth
  case result of
    Nothing -> throwError LoginErrorInvalidAuth
    Just (_, False) -> throwError LoginErrorEmailNotVerified
    Just (uId, _) -> lift $ newSession uId

resolveSessionId :: (SessionRepo m) => SessionId -> m (Maybe UserId)
resolveSessionId = findUserIdBySessionId

getUser :: (AuthRepo m) => UserId -> m (Maybe Email)
getUser = findEmailByUserId

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