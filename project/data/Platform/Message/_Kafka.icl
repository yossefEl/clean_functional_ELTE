implementation module Message._Kafka

import StdEnv

import Data.Error
import Data.Maybe
import System._Pointer

import code from library "-lrdkafka"

ERRSTR_SIZE :== 512

errstr :: String
errstr =: createArray ERRSTR_SIZE '\0'

rd_kafka_conf_new :: Pointer
rd_kafka_conf_new = code {
	ccall rd_kafka_conf_new ":p"
}

rd_kafka_conf_set :: !Pointer !String !String -> MaybeError String Pointer
rd_kafka_conf_set conf key val
	# r = set conf (packString key) (packString val) errstr ERRSTR_SIZE
	| r == 0
		= Ok conf
		= Error (unpackString errstr)
where
	set :: !Pointer !String !String !String !Int -> Int
	set _ _ _ _ _ = code {
		ccall rd_kafka_conf_set "psssI:I"
	}

rd_kafka_consumer_close :: !Pointer !.a -> .a
rd_kafka_consumer_close rk _ = code {
	ccall rd_kafka_consumer_close "p:V:A"
}

// - Returns `Ok ?None` when the result of poll is 0
// - Returns `Error` when a message with `err` set is returned
// - Otherwise, returns the pointer to the message
// 
// In other words, for a `Ok (?Just ..)` it can be assumed that `err = 0`.
rd_kafka_consumer_poll :: !Pointer !Int -> MaybeError String (?Pointer)
rd_kafka_consumer_poll rk timeout
	# rkw = poll rk timeout
	| rkw == 0
		= Ok ?None
	# err = derefInt rkw
	| err == 0
		= Ok (?Just rkw)
		= Error (rd_kafka_err2str err)
where
	poll :: !Pointer !Int -> Pointer
	poll _ _ = code {
		ccall rd_kafka_consumer_poll "pI:p"
	}

rd_kafka_destroy :: !Pointer !.a -> .a
rd_kafka_destroy rk _ = code {
	ccall rd_kafka_destroy "p:V:A"
}

rd_kafka_err2str :: !Int -> String
rd_kafka_err2str err = derefString (get err)
where
	get :: !Int -> Pointer
	get _ = code {
		ccall rd_kafka_err2str "I:p"
	}

rd_kafka_flush :: !Pointer !Int -> MaybeError Int ()
rd_kafka_flush rk timeout
	# r = flush rk timeout
	| r == 0
		= Ok ()
		= Error r
where
	flush :: !Pointer !Int -> Int
	flush _ _ = code {
		ccall rd_kafka_flush "GpI:I"
	}

rd_kafka_last_error :: Int
rd_kafka_last_error = code {
	ccall rd_kafka_last_error ":I"
}

rd_kafka_message_destroy :: !Pointer !.a -> .a
rd_kafka_message_destroy msg _ = code {
	ccall rd_kafka_message_destroy "p:V:A"
}

// type: 0 for producer, 1 for consumer
rd_kafka_new :: !Int !Pointer -> MaybeError String Pointer
rd_kafka_new type conf
	# rk = new type conf errstr ERRSTR_SIZE
	| rk <> 0
		= Ok rk
		= Error (unpackString errstr)
where
	new :: !Int !Pointer !String !Int -> Pointer
	new _ _ _ _ = code {
		ccall rd_kafka_new "IpsI:p"
	}

rd_kafka_poll :: !Pointer !Int -> Int
rd_kafka_poll rk timeout = code {
	ccall rd_kafka_poll "GpI:I"
}

rd_kafka_poll_set_consumer :: !Pointer -> MaybeError String ()
rd_kafka_poll_set_consumer client
	# err = set client
	| err == 0
		= Ok ()
		= Error (rd_kafka_err2str err)
where
	set :: !Pointer -> Int
	set _ = code {
		ccall rd_kafka_poll_set_consumer "p:I"
	}

rd_kafka_produce :: !Pointer !Int !Int !String !(?String) -> MaybeError String ()
rd_kafka_produce rkt partition flags payload key
	# key = fromMaybe "" key
	# r = produce rkt partition flags payload (size payload) key (size key) 0
	| r == 0
		= Ok ()
	# err = rd_kafka_last_error
	| err == err // force evaluation
		= Error (rd_kafka_err2str err)
		= abort "internal error in rd_kafka_produce\n"
where
	produce :: !Pointer !Int !Int !String !Int !String !Int !Pointer -> Int
	produce _ _ _ _ _ _ _ _ = code {
		ccall rd_kafka_produce "pIIsIsIp:I"
	}

rd_kafka_produce_batch :: !Pointer !Int !Int !{#Int} !Int -> Int
rd_kafka_produce_batch rkt partition flags messages cnt = code {
	ccall rd_kafka_produce_batch "pIIAI:I"
}

rd_kafka_subscribe :: !Pointer !Pointer -> MaybeError String ()
rd_kafka_subscribe rk list
	# err = subscribe rk list
	| err == 0
		= Ok ()
		= Error (rd_kafka_err2str err)
where
	subscribe :: !Pointer !Pointer -> Int
	subscribe _ _ = code {
		ccall rd_kafka_subscribe "pp:I"
	}

rd_kafka_topic_destroy :: !Pointer !.a -> .a
rd_kafka_topic_destroy topic _ = code {
	ccall rd_kafka_topic_destroy "p:V:A"
}

rd_kafka_topic_name :: !Pointer -> String
rd_kafka_topic_name topic = derefString (get topic)
where
	get :: !Pointer -> Pointer
	get _ = code {
		ccall rd_kafka_topic_name "p:p"
	}

rd_kafka_topic_new :: !Pointer !String !Pointer -> MaybeError String Pointer
rd_kafka_topic_new rk topic conf
	# topic = new rk (packString topic) conf
	| topic <> 0
		= Ok topic
	# err = rd_kafka_last_error
	| err == err // force evaluation
		= Error (rd_kafka_err2str err)
		= abort "internal error in rd_kafka_topic_new\n"
where
	new :: !Pointer !String !Pointer -> Pointer
	new _ _ _ = code {
		ccall rd_kafka_topic_new "psp:p"
	}

rd_kafka_topic_partition_list_add :: !Pointer !String !Int -> Pointer
rd_kafka_topic_partition_list_add list topic partition = add list (packString topic) partition
where
	add :: !Pointer !String !Int -> Pointer
	add _ _ _ = code {
		ccall rd_kafka_topic_partition_list_add "psI:p"
	}

rd_kafka_topic_partition_list_destroy :: !Pointer !.a -> .a
rd_kafka_topic_partition_list_destroy list _ = code {
	ccall rd_kafka_topic_partition_list_destroy "p:V:A"
}

rd_kafka_topic_partition_list_new :: !Int -> Pointer
rd_kafka_topic_partition_list_new cnt = code {
	ccall rd_kafka_topic_partition_list_new "I:p"
}
