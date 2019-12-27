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

type State = (PG.State, Redis.State, MQ.State, TVar M.State)
newtype App a = App
    { unApp :: ReaderT State IO a
    } deriving (Applicative, Functor, Monad, MonadReader State, MonadIO, MonadFail, E.MonadThrow, MonadUnliftIO)

run :: State -> App a -> IO a
run state = flip runReaderT state . unApp

instance AuthRepo App where
    addAuth = PG.addAuth
    setEmailAsVerified = PG.setEmailAsVerified
    findUserByAuth = PG.findUserByAuth
    findEmailFromUserId = PG.findEmailFromUserId

instance EmailVerificationNotif App where
    notifyEmailVerification = MQAuth.notifyEmailVerification

instance SessionRepo App where
    newSession = Redis.newSession
    findUserIdBySessionId = Redis.findUserIdBySessionId

action :: App ()
action = do
    let email = either undefined id $ mkEmail "cleancodematters1@gmail.com"
        passw = either undefined id $ mkPassword "Password1"
        auth = Auth email passw
    register auth
    Just vCode <- M.getVerificationCode email
    verifyEmail vCode
    Right session <- login auth
    Just uId <- resolveSessionId session     
    Just registeredEmail<- getUser uId
    print (session, uId, registeredEmail)   

main :: IO ()
main = do
    mState <- newTVarIO M.initialState
    PG.withState pgCfg $ \pgState ->
      Redis.withState redisCfg $ \redisState ->
        MQ.withState mqCfg 16 $ \ mqState -> do
            let runner = run (pgState, redisState, mqState, mState)
            MQAuth.init mqState runner
            runner action
    where
      mqCfg = "amqp://guest:guest@localhost:5672/%2F"
      redisCfg = "redis://localhost:6379/0"
      pgCfg = PG.Config 
              { PG.configUrl = "postgresql://localhost/hauth"
              , PG.configStripeCount = 2
              , PG.configMaxOpenConnPerStripe = 5
              , PG.configIdleConnTimeout = 10
              }