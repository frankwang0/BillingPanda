module Domain.Auth where
    
import ClassyPrelude

newtype Email = Email {emailRaw::Text} deriving (Show, Eq)

rawEmail :: Email -> Text
rawEmail = emailRaw

--mkEmail :: Text -> Either [Text] Email
--mkEmail = undefined

newtype Password = Password { passwordRaw::Text} deriving (Show, Eq)

rawPassword :: Password -> Text
rawPassword = passwordRaw

--mkPassword :: Text -> Either [Text] Password
--mkPassword = undefined

data Auth = Auth
  { authEmail :: Email
  , authPassword :: Password  
  } deriving (Show, Eq)