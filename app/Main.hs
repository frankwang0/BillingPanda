module Main where

import ClassyPrelude
import qualified Adapter.InMemory.Auth as M
import qualified Adapter.PostgreSQL.Auth as PG
import Domain.Auth
import Control.Monad.Reader
import Control.Monad.Fail
import qualified Control.Monad.Catch as E

type State = (PG.State, TVar M.State)
newtype App a = App
    { unApp :: ReaderT State IO a
    } deriving (Applicative, Functor, Monad, MonadReader State, MonadIO, MonadFail, E.MonadThrow)

run :: State -> App a -> IO a
run state = flip runReaderT state . unApp

instance AuthRepo App where
    addAuth = PG.addAuth
    setEmailAsVerified = PG.setEmailAsVerified
    findUserByAuth = PG.findUserByAuth
    findEmailFromUserId = PG.findEmailFromUserId

instance EmailVerificationNotif App where
    notifyEmailVerification = M.notifyEmailVerification

instance SessionRepo App where
    newSession = M.newSession
    findUserIdBySessionId = M.findUserIdBySessionId

action :: App ()
action = do
    let email = either undefined id $ mkEmail "FrankWangSydney@Gmail.com"
        passw = either undefined id $ mkPassword "Password1"
        auth = Auth email passw
    register auth
    Just vCode <- M.getNotificationForEmail email
    verifyEmail vCode
    Right session <- login auth
    Just uId <- resolveSessionId session     
    Just registeredEmail<- getUser uId
    print (session, uId, registeredEmail)   

main :: IO ()
main = do
    mState <- newTVarIO M.initialState 
    PG.withState config $ \pgState -> run (pgState, mState) action
    where 
        config = PG.Config
            { PG.configUrl = "postgresql://localhost/hauth"
            , PG.configStripeCount = 2
            , PG.configMaxOpenConnPerStripe =5
            , PG.configIdleConnTimeout = 10
            }