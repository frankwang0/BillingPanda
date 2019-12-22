module Adapter.Redis.Auth where

import ClassyPrelude
import qualified Domain.Auth as D
import Text.StringRandom
import Data.Has
import qualified Database.Redis as R
import qualified Control.Monad.Catch as E
import qualified Domain.Auth as D

type State = R.Connection

withState :: String -> (State -> IO a) -> IO a
withState connUrl action = 
    case R.parseConnectInfo connUrl of
        Left _ -> throwString "Invalid Redis conn URL"
        Right connInfo -> do
            conn <- R.checkedConnect connInfo
            action conn

type Redis r m = (Has State r, MonadReader r m, MonadIO m, E.MonadThrow m)

withConn :: Redis r m => R.Redis a -> m a
withConn action = do
    conn <- asks getter
    liftIO $ R.runRedis conn action

newSession :: Redis r m => D.UserId -> m D.SessionId
newSession userId = do
    sId <- liftIO $ stringRandomIO "[a-zA-Z0-9]{32}"
    result <- withConn $ R.Set (encodeUtf8 sId) (fromString . show $ userId)
    case result of 
        Right R.Ok -> return sId
        err -> throwString $ "Unexpected redis error: " <> show err