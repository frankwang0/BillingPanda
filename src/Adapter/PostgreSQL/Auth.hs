module Adapter.PostgreSQL.Auth where

import ClassyPrelude
import Data.Pool
import Database.PostgreSQL.Simple.Migration
import Database.PostgreSQL.Simple
import Data.Time
import Data.Has
import qualified Control.Monad.Catch as E

type PG r m = (Has State r, MonadReader r m, MonadIO m, E.MonadThrow m)

data Config = Config
    { configUrl :: ByteString
    , configStripeCount :: Int
    , configMaxOpenConnPerStripe :: Int
    , configIdleConnTimeout :: NominalDiffTime
    }

withPool :: Config -> (State -> IO a) -> IO a    
withPool cfg =
    bracket initPool cleanPool
    where 
        initPool = createPool openConn closeConn
                    (configStripeCount cfg)
                    (configIdleConnTimeout cfg)
                    (configMaxOpenConnPerStripe cfg)
        cleanPool = destroyAllResources
        openConn = connectPostgreSQL (configUrl cfg)
        closeConn = close

withState :: Config -> (State -> IO a) -> IO a
withState cfg action =
    withPool cfg $ \state -> do
        migrate state
        action state

type State = Pool Connection
migrate :: State -> IO ()
migrate pool = withResource pool $ \conn -> do
    result <- withTransaction conn (runMigrations False conn cmds)
    case result of 
        MigrationError err -> throwString err
        _ -> return ()
    where
        cmds = [ MigrationInitialization, MigrationDirectory "src/Adapter/PostgreSQL/Migrations"]
