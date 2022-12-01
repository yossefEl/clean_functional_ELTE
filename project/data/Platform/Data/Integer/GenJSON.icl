implementation module Data.Integer.GenJSON

import StdEnv
import Data.Integer, Text.GenJSON

JSONEncode{|Integer|} _ i = [JSONInt i.integer_s, JSONArray [JSONInt c \\ c <-: i.integer_a]]
JSONDecode{|Integer|} _ l=:[JSONInt s, JSONArray a:rest]
	| not (all (\x -> x=:(JSONInt _)) a)
		= (?None, l)
		= (?Just {integer_s=s, integer_a={c \\ JSONInt c <- a}}, rest)
JSONDecode{|Integer|} _ l = (?None, l)
