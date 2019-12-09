module Domain.Auth where
    
import ClassyPrelude

newtype Email = Email {emailRaw::Text} deriving (Show, Eq)

rawEmail :: Email -> Text
rawEmail = emailRaw