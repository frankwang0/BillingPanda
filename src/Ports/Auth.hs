module Ports.Auth where

import Domain.Auth
import ClassyPrelude

class Monad m => UserRepo m where
  addUser :: User -> m (Either RegistrationError (UserId, VerificationCode))
  setEmailAsVerified :: VerificationCode -> m (Either EmailVerificationError (UserId, Email))
  authenticate :: User -> m (Maybe (UserId, Bool))
  findEmailByUserId :: UserId -> m (Maybe Email)

class Monad m => EmailNotification m where
  sendVerificationEmail :: Email -> VerificationCode -> m ()

class Monad m => UserSessionRepo m where
  newSession :: UserId -> m SessionId
  findUserIdBySessionId :: SessionId -> m (Maybe UserId)