module Adapters.HTTP.Main where

-- import Domain.Auth
-- import Ports.Auth
-- import ClassyPrelude
-- import Web.Scotty.Trans
-- import Network.HTTP.Types.Status
-- import qualified Adapters.HTTP.API.Auth as AuthAPI
-- import Adapters.HTTP.Common
-- import Network.Wai
-- import Network.Wai.Middleware.Gzip

-- main :: (MonadIO m, UserRepo m, EmailNotification m, UserSessionRepo m)
--      => Int -> (m Response -> IO Response) -> IO ()
-- main port runner = scottyT port runner routes

-- routes :: (MonadIO m, UserRepo m, EmailNotification m, UserSessionRepo m)
--        => ScottyT LText m () 
-- routes = do
--     middleware $ gzip $ def { gzipFiles = GzipCompress }
--     AuthAPI.routes

--     defaultHandler $ \ e -> do
--         status status500
--         json ("InternalServerError" :: Text)