definition module Message._Kafka

/**
 * Low-level bindings for `Message.Kafka`.
 */

from StdInt import IF_INT_64_OR_32
from Data.Error import :: MaybeError
from System._Pointer import :: Pointer

rd_kafka_conf_new :: Pointer
rd_kafka_conf_set :: !Pointer !String !String -> MaybeError String Pointer
rd_kafka_consumer_close :: !Pointer !.a -> .a
rd_kafka_consumer_poll :: !Pointer !Int -> MaybeError String (?Pointer)
rd_kafka_destroy :: !Pointer !.a -> .a
rd_kafka_err2str :: !Int -> String
rd_kafka_flush :: !Pointer !Int -> MaybeError Int ()
rd_kafka_last_error :: Int
rd_kafka_message_destroy :: !Pointer !.a -> .a
rd_kafka_new :: !Int !Pointer -> MaybeError String Pointer
rd_kafka_poll :: !Pointer !Int -> Int
rd_kafka_poll_set_consumer :: !Pointer -> MaybeError String ()
rd_kafka_produce :: !Pointer !Int !Int !String !(?String) -> MaybeError String ()
rd_kafka_produce_batch :: !Pointer !Int !Int !{#Int} !Int -> Int
rd_kafka_subscribe :: !Pointer !Pointer -> MaybeError String ()
rd_kafka_topic_destroy :: !Pointer !.a -> .a
rd_kafka_topic_name :: !Pointer -> String
rd_kafka_topic_new :: !Pointer !String !Pointer -> MaybeError String Pointer
rd_kafka_topic_partition_list_add :: !Pointer !String !Int -> Pointer
rd_kafka_topic_partition_list_destroy :: !Pointer !.a -> .a
rd_kafka_topic_partition_list_new :: !Int -> Pointer
