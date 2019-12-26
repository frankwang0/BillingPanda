{-# LANGUAGE TemplateHaskell #-}
module Adapter.RabbitMQ.Auth where

import ClassyPrelude
import Adapter.RabbitMQ.Common
import qualified Adapter.InMemory.Auth as M
import Network.AMQP
import Data.Aeson
import Data.Aeson.TH
import qualified Domain.Auth as Data.Aeson.TH

data EmailVerificationPayload = EmailVerificationPayload
    { emailVerificationPayloadEmail :: Text
    , emailVerificationPayloadVerficationCode :: Text
    }

$(let structureName = fromMaybe "" . lastMay . splitElem '.' . show $ ''EmailVerificationPayload
        lowercaseFirst (x:xs) = toLower [x] <> xs
        lowercaseFirst xs = xs
        options = defaultOptions {fieldLabelModifier = lowercaseFirst . drop (length structName)}
    in deriveJSON options ''EmailVerificationPayload)