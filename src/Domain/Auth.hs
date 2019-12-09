module Domain.Auth where
    
import ClassyPrelude

newtype Email = Email {emailRaw::Text} deriving (Show, Eq)

rawEmail :: Email -> Text
rawEmail = emailRaw

data EmailValidationErr = EmailValidationErrInvalidEmail

--mkEmail :: Text -> Either [EmailValidationErr] Email
--mkEmail = undefined

newtype Password = Password { passwordRaw::Text} deriving (Show, Eq)

rawPassword :: Password -> Text
rawPassword = passwordRaw

data PasswordValidationErr = PasswordValidationErrLength Int
  | PasswordValidationErrMustContainUpperCase
  | PasswordValidationErrMustContainLowerCase
  | PasswordValidationErrMustContainNumber

--mkPassword :: Text -> Either [PasswordValidationErr] Password
--mkPassword = undefined

data Auth = Auth
  { authEmail :: Email
  , authPassword :: Password  
  } deriving (Show, Eq)