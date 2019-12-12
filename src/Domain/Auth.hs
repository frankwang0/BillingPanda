module Domain.Auth where
    
import ClassyPrelude
import Domain.Validation
import Control.Monad.Except

newtype Email = Email {emailRaw :: Text} deriving (Show, Eq, Ord)

rawEmail :: Email -> Text
rawEmail = emailRaw

data EmailVerificationError = EmailVerificationErrorInvalidCode

mkEmail :: Text -> Either [Text] Email
mkEmail val = Right $ Email val

newtype Password = Password { passwordRaw :: Text} deriving (Show, Eq)

rawPassword :: Password -> Text
rawPassword = passwordRaw

data PasswordValidationErr = PasswordValidationErrLength Int
  | PasswordValidationErrMustContainUpperCase
  | PasswordValidationErrMustContainLowerCase
  | PasswordValidationErrMustContainNumber

mkPassword :: Text -> Either [Text] Password
mkPassword = validate Password
  [
    lengthBetween 5 50 "Should be 5 and 50"
  ]

data Auth = Auth
  { authEmail :: Email
  , authPassword :: Password  
  } deriving (Show, Eq)

type VerificationCode = Text
type UserId = Int
type SessionId = Text

data RegistrationError = RegistrationErrorEmailTaken 
  deriving (Show, Eq)

class Monad m => AuthRepo m where
  addAuth :: Auth -> m (Either RegistrationError VerificationCode)
  setEmailAsVerified :: VerificationCode -> m (Either EmailVerificationError ())
  findUserByAuth :: Auth -> m (Maybe (UserId, Bool))
  findEmailFromUserId :: UserId -> m (Maybe Email)

class Monad m => EmailVerificationNotif m where
  notifyEmailVerification :: Email -> VerificationCode -> m ()

class (Monad m) => SessionRepo m where
  newSession :: UserId -> m SessionId
  findUserIdBySessionId :: SessionId -> m (Maybe UserId)  

register :: (AuthRepo m, EmailVerificationNotif m) => Auth -> m (Either RegistrationError ())
register auth = runExceptT $ do
  vCode <- ExceptT $ addAuth auth
  let email = authEmail auth
  lift $ notifyEmailVerification email vCode