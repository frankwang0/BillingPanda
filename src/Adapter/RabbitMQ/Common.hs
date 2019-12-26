module Adapter.RabbitMQ.Common where

import ClassyPrelude
import Network.AMQP
import Data.Has
import Data.Aeson

data State = State
    { statePublisherChan :: Channel
    , stateConsumerChan :: Channel
    }
type Rabbit r m = (Has State r, MonadReader r m, MonadIO m)

withState :: String -> Integer -> (State -> IO a) -> IO a
withState connUri prefetchCount action = ClassyPrelude.bracket initState destroyState action'
    where 
        initState = do
            publisher <- openConnAndChann
            consumer <- openConnAndChann
            return (publisher, consumer)

        openConnAndChann = do
            conn <- openConnection'' . fromURI  $ connUri
            chan <- openChannel conn
            confirmSelect chan False
            qos chan 0 (fromInteger prefetchCount) True
            return (conn, chan)

        destroyState((conn1, _), (conn2, _)) = do
            closeConnection conn1
            closeConnection conn2

        action' ((_, pubChan), (_, conChan)) = action (State pubChan conChan)

initExchange :: State -> Text -> IO ()
initExchange (State pubChan _) exchangeName = do
    let exchange = newExchange { exchangeName = exchangeName, exchangeType = "topic"}
    declareExchange pubChan exchange

initQueue :: State -> Text -> Text -> Text -> IO ()
initQueue state@(State pubChan _) queueName exchangeName routingKey = do
    initExchange state exchangeName
    void $ declareQueue pubChan (newQueue { queueName = queueName})
    bindQueue pubChan queueName exchangeName routingKey

initConsumer :: State -> Text -> (Message -> IO Bool) -> IO ()
initConsumer (State _ conChan) queueName handler = 
    void . consumeMsgs conChan queueName Ack $ \(msg, env) -> do
        result <- handler msg
        if result then ackEnv env else rejectEnv env False 

publish :: (ToJSON a, Rabbit r m) => Text -> Text -> a -> m ()
publish exchange routingKey payload = do
    (State chan _) <- asks getter
    let msg = newMsg { msgBody = encode payload }
    liftIO . void $ publishMsg chan exchange routingKey msg

consumeAndProcess :: (FromJSON a, MonadCatch m)
                  => Message -> (a -> m Bool) -> m Bool
consumeAndProcess msg handler =
    case eitherDecode' (msgBody msg) of
        Left err -> return False
        Right payload -> do
            result <- tryAny (handler payload)
            case result of
                Left err -> return False
                Right bool -> return bool