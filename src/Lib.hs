module Lib
    ( someFunc
    ) where

import ClassyPrelude
import Domain.Auth
import qualified Adapter.InMemory.Auth as M
import Control.Monad.Reader

type State = TVar M.State
newtype App a = App
    { unApp :: ReaderT State IO a        
    } deriving (Applicative, Functor, Monad, MonadReader State, MonadIO)

run :: State -> App a -> IO a
run state = flip runReaderT state . unApp    

instance AuthRepo App where
    addAuth = M.addAuth
    setEmailAsVerified = M.setEmailAsVerified
    findUserByAuth = M.findUserByAuth
    findEmailFromUserId = M.findEmailFromUserId

instance EmailVerificationNotif App where
    notifyEmailVerification = M.notifyEmailVerification

instance SessionRepo App where
    newSession = M.newSession
    findUserIdBySessionId = M.findUserIdBySessionId

someFunc :: IO ()
someFunc = do
    state <- newTVarIO M.initialState
    run state action

action :: App ()
action = do
    let email = either undefined id $ mkEmail "frank.wang@servcorp.com.au"
        passw = either undefined id $ mkPassword "Password1"
        auth = Auth email passw

    register auth
    Just vCode <- M.getNotificationForEmail email
    verifyEmail vCode
    Right session <- login auth
    Just uId <- resolveSessionId session
    Just registeredEmail <- getUser uId
    print (session, uId, registeredEmail)
