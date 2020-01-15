module Domain.Auth where

import ClassyPrelude
import Domain.Validation
import Control.Monad.Except

newtype Email = Email {emailRaw :: Text} deriving (Show, Eq, Ord)
rawEmail :: Email -> Text
rawEmail = emailRaw

mkEmail :: Text -> Either [Text] Email
mkEmail val = Right $ Email val

newtype Password = Password { passwordRaw :: Text} deriving (Show, Eq)
rawPassword :: Password -> Text
rawPassword = passwordRaw

mkPassword :: Text -> Either [Text] Password
mkPassword = validate Password
  [
    lengthBetween 5 50 "Should be 5 and 50"
  ]

data EmailVerificationError = EmailVerificationErrorInvalidCode deriving (Show, Eq)

data LoginError = LoginErrorInvalidAuth | LoginErrorEmailNotVerified deriving (Show, Eq)

data PasswordValidationErr = PasswordValidationErrLength Int
  | PasswordValidationErrMustContainUpperCase
  | PasswordValidationErrMustContainLowerCase
  | PasswordValidationErrMustContainNumber

data User = User
  { authEmail :: Email
  , authPassword :: Password
  } deriving (Show, Eq)

type VerificationCode = Text
type UserId = Int
type SessionId = Text

data RegistrationError = RegistrationErrorEmailTaken
  deriving (Show, Eq)