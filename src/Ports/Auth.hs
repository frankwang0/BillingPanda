module Ports.Auth where

import Domain.Auth
import ClassyPrelude

class Monad m => UserRepo m where
  addAuth :: User -> m (Either RegistrationError (UserId, VerificationCode))
  setEmailAsVerified :: VerificationCode -> m (Either EmailVerificationError (UserId, Email))
  findUserByAuth :: User -> m (Maybe (UserId, Bool))
  findEmailByUserId :: UserId -> m (Maybe Email)

class Monad m => EmailNotification m where
  sendVerificationEmail :: Email -> VerificationCode -> m ()

class (Monad m) => SessionRepo m where
  newSession :: UserId -> m SessionId
  findUserIdBySessionId :: SessionId -> m (Maybe UserId)