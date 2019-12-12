module Lib
    ( someFunc
    ) where

import ClassyPrelude
import Domain.Auth

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- instance AuthRepo IO where
--     addAuth (Auth email pass) = do
--         putStrLn $ "adding auth: " <> rawEmail email
--         return $ Right "fake verification code"

-- instance EmailVerificationNotif IO where
--     notifyEmailVerification email vcode = 
--         putStrLn $ "Notify " <> rawEmail email <> " - " <> vcode


