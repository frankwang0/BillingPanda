{-# LANGUAGE TemplateHaskell #-}
module Adapters.RabbitMQ.Auth where

import ClassyPrelude
import Adapters.RabbitMQ.Common
import qualified Adapters.InMemory.Auth as M
import Network.AMQP
import Data.Aeson
import Data.Aeson.TH
import qualified Domain.Auth as D

data EmailVerificationPayload = EmailVerificationPayload
    { email :: Text
    , verficationCode :: Text
    }

$(let structName = fromMaybe "" . lastMay . splitElem '.' . show $ ''EmailVerificationPayload 
      lowercaseFirst (x:xs) = toLower [x] <> xs
      lowercaseFirst xs = xs
      options = defaultOptions {fieldLabelModifier = lowercaseFirst . drop (length structName)} 
    in  deriveJSON options ''EmailVerificationPayload)

sendVerificationEmail :: (Rabbit r m)
                        => D.Email -> D.VerificationCode -> m ()
sendVerificationEmail email vCode = 
    let payload = EmailVerificationPayload (D.rawEmail email) vCode
    in publish "auth" "userRegistered" payload

consumeEmailVerification :: (M.InMemory r m, MonadUnliftIO m) 
                         => (m Bool -> IO Bool) -> Message -> IO Bool
consumeEmailVerification runner msg =
    runner $ consumeAndProcess msg handler
    where
        handler payload =
            case D.mkEmail (email payload) of
                Left err -> return False 
                Right email -> do
                    let vCode = verficationCode payload
                    M.sendVerificationEmail email vCode
                    return True

init :: (M.InMemory r m , MonadUnliftIO m) => State -> (m Bool -> IO Bool) -> IO ()
init state runner = do
    initQueue state "verifyEmail" "auth" "userRegistered"
    initConsumer state "verifyEmail" (consumeEmailVerification runner)