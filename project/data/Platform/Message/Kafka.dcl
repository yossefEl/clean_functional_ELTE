definition module Message.Kafka

/**
 * Bindings for Apache Kafka. See:
 *
 * - https://kafka.apache.org/intro.html
 * - https://github.com/edenhill/librdkafka
 * - https://github.com/edenhill/librdkafka/wiki/Language-bindings-development
 *
 * Not all functionality has been implemented yet.
 *
 * A basic producer can be implemented as follows, based on
 * https://github.com/edenhill/librdkafka/blob/master/examples/producer.c:
 *
 * ```clean
 * //* Create a producer and write "payload" to the topic "topic".
 * producer :: !*World -> *World
 * producer w
 *     # c = newKafkaClient KafkaProducer [("bootstrap.servers", "localhost")]
 *     | c=:(Error _) = abort (fromError c+++"\n")
 *     # c = fromOk c
 *     # t = newKafkaTopic "topic" c
 *     | t=:(Error _) = abort (fromError t+++"\n")
 *     # t = fromOk t
 *     # (err,w) = produceKafkaMessage KafkaUnassignedPartition ?None "payload" t w
 *     | err=:(Error _) = abort (fromError err+++"\n")
 *     # (err,w) = flushKafkaClient 1000 c w
 *     | err=:(Error _) = abort (fromError err+++"\n")
 *     | otherwise = w
 * ```
 *
 * A basic consumer can be implemented as follows, based on
 * https://github.com/edenhill/librdkafka/blob/master/examples/consumer.c:
 *
 * ```clean
 * //* Print messages from the topic "topic" to stdout.
 * consumer :: !*World -> *World
 * consumer w
 *     # c = newKafkaClient KafkaConsumer
 *         [ ("bootstrap.servers", "localhost")
 *         , ("group.id", "group")
 *         , ("auto.offset.reset", "earliest")
 *         ]
 *     | c=:(Error _) = abort (fromError c+++"\n")
 *     # c = fromOk c
 *     # (err, w) = redirectKafkaPollQueue c w
 *     | err=:(Error _) = abort (fromError err+++"\n")
 *     # (err, w) = subscribeToKafkaTopics [("topic", KafkaUnassignedPartition)] c w
 *     | err=:(Error _) = abort (fromError err+++"\n")
 *     # w = loop c w
 *     # w = closeKafkaConsumer c w
 *     # w = destroyKafkaClient c w
 *     = w
 * where
 *     loop c w
 *         # (mbMsg,w) = pollKafkaConsumer 100 c w
 *         | mbMsg=:(Error _)
 *             # err = fromError mbMsg
 *             | err == "Broker: No more messages" // ignore this error
 *                 = loop c w
 *             # (_,w) = fclose (stderr <<< err <<< "\n") w
 *             = loop c w
 *         # mbMsg = fromOk mbMsg
 *         | isNone mbMsg
 *             = loop c w
 *         # msg = fromJust mbMsg
 *         # (io,w) = stdio w
 *         # io = io <<< "Message: " <<< msg.payload <<< "\n"
 *         # (_,w) = fclose io w
 *         = loop c w
 * ```
 */

from Data.Error import :: MaybeError
from System._Pointer import :: Pointer

:: KafkaClientType
	= KafkaConsumer
	| KafkaProducer

:: KafkaClient (:== Pointer)

:: KafkaTopic (:== Pointer)

:: KafkaMessage =
	{ topic     :: !KafkaTopic
	, partition :: !KafkaPartition
	, offset    :: !Int //* NB: on 32-bit systems, the upper 32 bits of the offset are lost.
	, payload   :: !String
	, key       :: !String
	}

/**
 * Messages can be sent to a particular (non-negative) partition identifier, or
 * to the unassigned partition `KafkaUnassignedPartition`.
 */
:: KafkaPartition :== Int
KafkaUnassignedPartition :== -1

/**
 * Polling can either:
 *
 * - Not block at all (`KafkaNoBlock`)
 * - Block until new events are available (`KafkaNoTimeout`)
 * - Block for a certain time only (given in ms)
 */
:: KafkaBlockSetting :== Int
KafkaNoBlock   :== 0
KafkaNoTimeout :== -1

/**
 * Creates a new client given a list of settings.
 *
 * @param The kind of client.
 * @param Setings.
 * @result A new `KafkaClient`, or an error on failure.
 */
newKafkaClient :: !KafkaClientType ![(String,String)] -> MaybeError String KafkaClient

/**
 * Let a consumer leave the group gracefully before destroying it with
 * `destroyKafkaClient`.
 */
closeKafkaConsumer :: !KafkaClient !*World -> *World

/**
 * Destroy a `KafkaClient`.
 *
 * Before destroying a client, all remaining messages should be flushed with
 * `flushKafkaClient` and a reasonably long timeout.
 *
 * A consumer should be closed with `closeKafkaConsumer` before destroying it.
 */
destroyKafkaClient :: !KafkaClient !*World -> *World

/**
 * Poll for new events.
 *
 * This function should be called regularly.
 *
 * @param Whether to block and for how long.
 * @param The client.
 * @result The number of events served.
 */
pollKafkaClient :: !KafkaBlockSetting !KafkaClient !*World -> (!Int, !*World)

/**
 * Attempt to flush all remaning events.
 *
 * This function should be called before destroying the client with
 * `destroyKafkaClient`.
 *
 * @param Whether to block and for how long.
 * @param The client.
 * @result The number of events served.
 */
flushKafkaClient :: !KafkaBlockSetting !KafkaClient !*World -> (!MaybeError String (), !*World)

/**
 * Create a new topic.
 *
 * Topics are refcounted internally. Creating a new topic with a name that has
 * already been used previously will return the previous handle.
 *
 * NB: configurations (`rd_kafka_topic_conf_set`) are not supported yet.
 *
 * @param The topic name.
 * @param The client.
 * @result The new topic.
 */
newKafkaTopic :: !String !KafkaClient -> MaybeError String KafkaTopic

/**
 * Destroy a topic.
 *
 * When a topic has been 'created' multiple times (i.e. `newKafkaTopic` was
 * used multiple times with the same name), this does not actually free
 * resources but only decreases the topic's refcount.
 */
destroyKafkaTopic :: !KafkaTopic !*World -> *World

//* Get the name of a topic.
kafkaTopicName :: !KafkaTopic -> String

/**
 * Produce and queue a message.
 *
 * `pollKafkaClient` must be called after this function to ensure the message
 * is sent as well.
 *
 * NB: for now it is not possible to check whether a message has been sent
 * successfully.
 *
 * @param The partition to send to, or `KafkaUnassignedPartition`.
 * @param An optional message key.
 * @param The message payload.
 * @param The topic to send to.
 * @result On failure, a human-readable error message is returned.
 */
produceKafkaMessage :: !KafkaPartition !(?String) !String !KafkaTopic !*World -> (!MaybeError String (), !*World)

/**
 * Subscribe to a list of topics.
 *
 * @param The topics to subscribe to.
 * @param The client (should be a `KafkaConsumer`).
 * @result On failure, a human-readable error message is returned.
 */
subscribeToKafkaTopics :: ![(String,KafkaPartition)] !KafkaClient !*World -> (!MaybeError String (), !*World)

/**
 * Redirect the main Kafka poll queue to the poll queue of a particular client
 * (should be a `KafkaConsumer`). It is not permitted to use `pollKafkaClient`
 * after `redirectKafkaPollQueue`.
 */
redirectKafkaPollQueue :: !KafkaClient !*World -> (!MaybeError String (), !*World)

/**
 * Poll for new events.
 *
 * This function should be called regularly.
 *
 * @param Whether to block and for how long.
 * @param The client.
 * @result The number of events served.
 */
pollKafkaConsumer :: !KafkaBlockSetting !KafkaClient !*World -> (!MaybeError String (?KafkaMessage), !*World)
