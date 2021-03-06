module Adapters.InMemory.Auth where

import ClassyPrelude
import qualified Domain.Auth as D
import Data.Has
import Text.StringRandom
import Control.Monad.Except

type InMemory r m = (Has (TVar State) r, MonadReader r m, MonadIO m)

data State = State
    { stateAuths :: [(D.UserId, D.User)]
    , stateUnverifiedEmails :: Map D.VerificationCode D.Email
    , stateVerifiedEmails :: Set D.Email
    , stateUserIdCounter :: Int
    , stateNotifications :: Map D.Email D.VerificationCode
    , stateSessions :: Map D.SessionId D.UserId
    }

initialState :: State
initialState = State
    { stateAuths = []
    , stateUnverifiedEmails = mempty
    , stateVerifiedEmails = mempty
    , stateUserIdCounter = 0
    , stateNotifications = mempty
    , stateSessions = mempty
    }

addAuth :: InMemory r m
        => D.User
        -> m (Either D.RegistrationError (D.UserId, D.VerificationCode))
addAuth user = do
    tvar <- asks getter
    vCode <- liftIO $ stringRandomIO "[A-Za-z0-9]{16}"
    atomically . runExceptT $ do
        state <- lift $ readTVar tvar
        let auths = stateAuths state
            email = D.authEmail user
            isDuplicate = elem email . map (D.authEmail . snd) $ auths
        when isDuplicate $ throwError D.RegistrationErrorEmailTaken

        let newUserId = stateUserIdCounter state + 1
            newAuths = (newUserId, user) : auths
            unverifieds = stateUnverifiedEmails state
            newUnVerifieds = insertMap vCode email unverifieds
            newState = state
                { stateAuths = newAuths
                , stateUserIdCounter = newUserId
                , stateUnverifiedEmails = newUnVerifieds
                }

        lift $ writeTVar tvar newState
        return (newUserId, vCode)

setEmailAsVerified :: InMemory r m
                    => D.VerificationCode
                    -> m (Either D.EmailVerificationError (D.UserId, D.Email))
setEmailAsVerified vCode = do
    tvar <- asks getter
    atomically . runExceptT $ do
        state <- lift $ readTVar tvar

        let unverifieds = stateUnverifiedEmails state
            mayEmail = lookup vCode unverifieds
        email <- mayEmail `orThrow` D.EmailVerificationErrorInvalidCode

        let auths = stateAuths state
            mayUserId = map fst . find ((email ==) . D.authEmail . snd) $ auths
        uId <- mayUserId `orThrow` D.EmailVerificationErrorInvalidCode

        let verifieds = stateVerifiedEmails state
            newUnverifieds = deleteMap vCode unverifieds
            newVerifieds = insertSet email verifieds
            newState = state
                { stateUnverifiedEmails = newUnverifieds
                , stateVerifiedEmails = newVerifieds
                }

        lift $ writeTVar tvar newState
        return (uId, email)

authenticate :: InMemory r m => D.User -> m (Maybe (D.UserId, Bool))
authenticate user = do
    tvar <- asks getter
    state <- liftIO $ readTVarIO tvar
    let mayUserId = map fst . find ((user ==) . snd) $ stateAuths state
    case mayUserId of
        Nothing -> return Nothing
        Just uId -> do
            let verifieds = stateVerifiedEmails state
                email = D.authEmail user
                isVerified = elem email verifieds
            return $ Just (uId, isVerified)

findEmailByUserId :: InMemory r m => D.UserId -> m (Maybe D.Email)
findEmailByUserId userId = do
    tvar <- asks getter
    state <- liftIO $ readTVarIO tvar
    let myAuth = map snd . find ((userId ==) .fst) $ stateAuths state
    return $ D.authEmail <$> myAuth

getVerificationCode :: InMemory r m => D.Email -> m (Maybe D.VerificationCode)
getVerificationCode email = do
    tvar <- asks getter
    state <- liftIO $ readTVarIO tvar
    return $ lookup email $ stateNotifications state

sendVerificationEmail :: InMemory r m => D.Email -> D.VerificationCode -> m ()
sendVerificationEmail email verificationCode = do
    tvar <- asks getter
    atomically $ do
        state <- readTVar tvar
        let notifications = stateNotifications state
            newNotifications = insertMap email verificationCode notifications
            newState = state { stateNotifications = newNotifications}
        writeTVar tvar newState

newSession :: InMemory r m => D.UserId -> m D.SessionId
newSession uId = do
    tvar <- asks getter
    sId <- liftIO $ (tshow uId <>) <$> stringRandomIO "[A-Za-z0-9]{16}"
    atomically $ do
        state <- readTVar tvar
        let sessions = stateSessions state
            newSessions = insertMap sId uId sessions
            newState = state {stateSessions = newSessions}
        writeTVar tvar newState
        return sId

findUserIdBySessionId :: InMemory r m => D.SessionId -> m (Maybe D.UserId)
findUserIdBySessionId sId = do
    tvar <- asks getter
    liftIO $ lookup sId . stateSessions <$> readTVarIO tvar

orThrow :: MonadError e m => Maybe a -> e -> m a
orThrow Nothing e = throwError e
orThrow (Just a) _ = return a