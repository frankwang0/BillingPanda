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
someFunc = putStrLn "someFunc"
