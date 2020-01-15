module Adapters.PostgreSQL.Auth where

import ClassyPrelude
import Data.Pool
import Database.PostgreSQL.Simple.Migration
import Database.PostgreSQL.Simple
import Data.Time
import Data.Has
import qualified Control.Monad.Catch as E
import qualified Domain.Auth as D
import Text.StringRandom

type PG r m = (Has State r, MonadReader r m, MonadIO m, E.MonadThrow m)

type State = Pool Connection

data Config = Config
    { configUrl :: ByteString
    , configStripeCount :: Int
    , configMaxOpenConnPerStripe :: Int
    , configIdleConnTimeout :: NominalDiffTime
    }

withConn :: PG r m => (Connection -> IO a) -> m a
withConn action = do
    pool <- asks getter
    liftIO . withResource pool $ \ conn -> action conn

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

migrate :: State -> IO ()
migrate pool = withResource pool $ \conn -> do
    result <- withTransaction conn (runMigrations False conn cmds)
    case result of
        MigrationError err -> throwString err
        _ -> return ()
    where
        cmds = [ MigrationInitialization, MigrationDirectory "src/Adapters/PostgreSQL/Migrations"]

addAuth :: PG r m 
        => D.User 
        -> m (Either D.RegistrationError (D.UserId, D.VerificationCode))
addAuth (D.User email pass) = do
    let rawEmail = D.rawEmail email
        rawPassw = D.rawPassword pass
    vCode <- liftIO $ do
        r <- stringRandomIO "[A-Za-z0-9]{16}"
        return $ tshow rawEmail <> "_" <> r

    result <- withConn $ \conn -> try $ query conn sql (rawEmail, rawPassw, vCode)
    case result of
        Right [Only uId] -> return $ Right (uId, vCode)
        Right _ -> throwString "Should not happen: PG doesn't return userId"
        Left err@SqlError{sqlState = state, sqlErrorMsg = msg} ->
            if state == "23505" && "auths_email_key" `isInfixOf` msg
                then return $ Left D.RegistrationErrorEmailTaken
                else throwString $ "Unhandled PG exception: " <> show err
    where
        sql = "insert into auths \
            \(email, pass, email_verification_code, is_email_verified)\
            \values(?, crypt(?,gen_salt('bf')), ?, 'f') returning id"

setEmailAsVerified :: PG r m 
                    => D.VerificationCode 
                    -> m (Either D.EmailVerificationError (D.UserId, D.Email)) 
setEmailAsVerified vCode = do
    result <- withConn $ \conn -> query conn sql (Only vCode)        
    case result of
        [(uId, mail)] -> case D.mkEmail  mail of
            Right email -> return $ Right (uId, email)   
            _ -> throwString $ "Should not happen: email in DB is not valid: " <> unpack mail
        _ -> return $ Left D.EmailVerificationErrorInvalidCode
    where sql = "update auths \
                \set is_email_verified = 't' \
                \where email_verification_code = ? \
                \returning id, cast (email as text)"

findUserByAuth :: PG r m => D.User -> m (Maybe (D.UserId, Bool))
findUserByAuth (D.User email pass) = do
    let rawEmail = D.rawEmail email
        rawPassw = D.rawPassword pass
    result <- withConn $ \conn -> query conn sql (rawEmail, rawPassw)
    return $ case result of
        [(uId, isVerified)] -> Just (uId, isVerified)
        _ -> Nothing
    where sql = "select id , is_email_verified \
                \from auths \
                \where email = ? and pass = crypt(?,pass)"

findEmailByUserId :: PG r m => D.UserId -> m (Maybe D.Email)
findEmailByUserId uId = do
    result <- withConn $ \conn -> query conn sql (Only uId)
    case result of
        [Only mail] -> case D.mkEmail mail of
            Right email -> return $ Just email
            _ -> throwString $ "Should not happen: email in DB is not valid: " <> unpack mail
    where sql = "select cast(email as text) from auths where id = ?"