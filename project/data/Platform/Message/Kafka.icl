implementation module Message.Kafka

import StdEnv

import Control.Monad
import Data.Error
import Data.Maybe
import Message._Kafka
import System._Pointer
import qualified Text
from Text import class Text, instance Text String

:: KafkaClient :== Pointer

:: KafkaTopic :== Pointer

// Offsets to the message struct
KM_Err       :== 0
KM_Topic     :== IF_INT_64_OR_32  8  4
KM_Partition :== IF_INT_64_OR_32 16  8
KM_Payload   :== IF_INT_64_OR_32 24 12
KM_Len       :== IF_INT_64_OR_32 32 16
KM_Key       :== IF_INT_64_OR_32 40 20
KM_KeyLen    :== IF_INT_64_OR_32 48 24
KM_Offset    :== IF_INT_64_OR_32 56 28
KM_Private   :== IF_INT_64_OR_32 64 36

newKafkaClient :: !KafkaClientType ![(String,String)] -> MaybeError String KafkaClient
newKafkaClient type settings =
	foldM
		(\c (k,v) -> rd_kafka_conf_set c k v)
		rd_kafka_conf_new
		settings >>=
	rd_kafka_new (if type=:KafkaProducer 0 1)

closeKafkaConsumer :: !KafkaClient !*World -> *World
closeKafkaConsumer client w = rd_kafka_consumer_close client w

destroyKafkaClient :: !KafkaClient !*World -> *World
destroyKafkaClient client w = rd_kafka_destroy client w

pollKafkaClient :: !KafkaBlockSetting !KafkaClient !*World -> (!Int, !*World)
pollKafkaClient block client w
	# nevents = rd_kafka_poll client block
	= (nevents, w)

flushKafkaClient :: !KafkaBlockSetting !KafkaClient !*World -> (!MaybeError String (), !*World)
flushKafkaClient block client w
	# ok = rd_kafka_flush client block
	| ok=:(Error _)
		= (Error (rd_kafka_err2str (fromError ok)), w)
		= (Ok (), w)

newKafkaTopic :: !String !KafkaClient -> MaybeError String KafkaTopic
newKafkaTopic topic client = rd_kafka_topic_new client topic 0

destroyKafkaTopic :: !KafkaTopic !*World -> *World
destroyKafkaTopic topic w = rd_kafka_topic_destroy topic w

kafkaTopicName :: !KafkaTopic -> String
kafkaTopicName topic = rd_kafka_topic_name topic

produceKafkaMessage :: !KafkaPartition !(?String) !String !KafkaTopic !*World -> (!MaybeError String (), !*World)
produceKafkaMessage partition key msg topic w
	# flags = 2 /* F_COPY to copy payload */
	= (rd_kafka_produce topic partition flags msg key, w)

subscribeToKafkaTopics :: ![(String,KafkaPartition)] !KafkaClient !*World -> (!MaybeError String (), !*World)
subscribeToKafkaTopics topics client w
	# list = foldl
		(\l (t,p)
			# r = rd_kafka_topic_partition_list_add l t p
			| r <> r // force evaluation
				-> l
				-> l)
		(rd_kafka_topic_partition_list_new (length topics))
		topics
	# err = rd_kafka_subscribe client list
	# w = forceEval err w
	# w = rd_kafka_topic_partition_list_destroy list w
	= (err, w)

redirectKafkaPollQueue :: !KafkaClient !*World -> (!MaybeError String (), !*World)
redirectKafkaPollQueue client w = (rd_kafka_poll_set_consumer client, w)

pollKafkaConsumer :: !KafkaBlockSetting !KafkaClient !*World -> (!MaybeError String (?KafkaMessage), !*World)
pollKafkaConsumer block client w
	# mbMsg = rd_kafka_consumer_poll client block
	| mbMsg=:(Error _)
		= (liftError mbMsg, w)
	# mbMsg = fromOk mbMsg
	| isNone mbMsg
		= (Ok ?None, w)
	# ptr = fromJust mbMsg
	  payload = readInt ptr KM_Payload
	  key = readInt ptr KM_Key
	# msg =
		{ topic     = readInt ptr KM_Topic
		, partition = readInt ptr KM_Partition
		, offset    = readInt ptr KM_Offset
		, payload   = if (payload == 0) "" (derefCharArray payload (readInt ptr KM_Len))
		, key       = if (key == 0)     "" (derefCharArray key     (readInt ptr KM_KeyLen))
		}
	# msg = rd_kafka_message_destroy ptr msg
	= (Ok (?Just msg), w)
