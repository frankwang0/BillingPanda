module Lib
  ( someFunc
  ) where

import ClassyPrelude
import qualified Adapter.InMemory.Auth as M
import Domain.Auth
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
  let email = either undefined id $ mkEmail "ecky@test.com"
      passw = either undefined id $ mkPassword "1234ABCDefgh"
      auth = Auth email passw
  register auth

  notification <- M.getNotificationForEmail email
  vCode <- case notification of 
      Just n -> pure n
      Nothing -> error "no notification found"

  verifyEmail vCode
  loginSession <- login auth
  session <- case loginSession of
      Right s -> pure s
      Left _ -> error "login failed"

  userId <- resolveSessionId session
  uId <- case userId of 
      Just u -> pure u
      Nothing -> error "no userId found"      

  userEmail<- getUser uId
  registeredEmail <- case userEmail of
      Just e -> pure e
      Nothing -> error "no email found"
  print (session, uId, registeredEmail)