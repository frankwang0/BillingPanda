module Main where

import ClassyPrelude
import Domain.Auth
import Ports.Auth
import Usecases.Auth
import Control.Monad.Reader
import Text.StringRandom
import qualified Adapters.RabbitMQ.Auth as MQAuth
import qualified Adapters.HTTP.Main as HTTP
import qualified Adapters.InMemory.Auth as M

main :: IO ()
main =
    withState $ \ port state@(_,_,rabbitState,_) -> do
        let runner = run state
        MQAuth.init rabbitState runner
        runner action
        -- HTTP.main port runner

action :: App ()
action = do
    randEmail <- liftIO $ stringRandomIO "[a-z0-9]{5}@gmail\\.com"
    let email = either undefined id $ mkEmail randEmail
        passw = either undefined id $ mkPassword "Password1"
        auth = Auth email passw
    register auth
    verificationCode <- pollCode email
    verifyEmail verificationCode
    Right session <- login auth
    Just uId <- resolveSessionId session
    Just registeredEmail<- getUser uId
    print (session, uId, registeredEmail)
    where
        pollCode email = do
            result <- M.getVerificationCode email
            case result of
                Nothing -> pollCode email
                Just vCode -> return vCode             