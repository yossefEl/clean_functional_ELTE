implementation module Data.Encoding.GenBinary

import StdGeneric, StdEnv, StdOverloadedList
import Data._Array, Data.Func, Data.Maybe, Data.Functor, Data.Tuple, Data.Array
import System._Unsafe

decode :: !{#Char} -> ?a | gBinaryDecode{|*|} a
decode binary = fst $ gBinaryDecode{|*|} $ mkEncodingSt {x \\ x <-: binary}

encode :: !a -> {#Char} | gBinaryEncodingSize{|*|}, gBinaryEncode{|*|} a
encode x
	#! encoded_size = gBinaryEncodingSize{|*|} x 0
	#! arr_size = (encoded_size+7) >> 3
	#! bits = createArray arr_size '\0'
	= (gBinaryEncode{|*|} x (mkEncodingSt bits)).es_bits

mkEncodingSt :: !*{#Char} -> *EncodingSt
mkEncodingSt arr = {es_pos = 0, es_bits = arr, es_cons_path=[]}

generic gBinaryEncode a :: !a !*EncodingSt -> *EncodingSt
gBinaryEncode{|Int|}    x                st = encodeInt x st
gBinaryEncode{|Real|}   x                st = encodeReal x st
gBinaryEncode{|Char|}   x                st = encodeChar x st
gBinaryEncode{|Bool|}   x                st = encodeBool x st
gBinaryEncode{|String|} xs               st = encodeArray encodeChar xs st
gBinaryEncode{|UNIT|}   _                st = st
gBinaryEncode{|PAIR|}   cx cy (PAIR x y) st = cy y $ cx x st
gBinaryEncode{|EITHER|} cl cr (LEFT x)   st = cl x st
gBinaryEncode{|EITHER|} cl cr (RIGHT x)  st = cr x st
gBinaryEncode{|CONS of d|} c (CONS x)    st = c x $ encodeIntUsingNBits (ceil_log2 0 d.gcd_type_def.gtd_num_conses) d.gcd_index st
gBinaryEncode{|FIELD|}  c (FIELD x)      st = c x st
gBinaryEncode{|OBJECT|} c (OBJECT x)     st = c x st
gBinaryEncode{|RECORD|} c (RECORD x)     st = c x st
gBinaryEncode{|{}|}     c xs             st = encodeArray c xs st
gBinaryEncode{|{!}|}    c xs             st = encodeArray c xs st
gBinaryEncode{|[]|}     c xs             st = encodeList c xs st
gBinaryEncode{|[!]|}    c xs             st = encodeList c xs st
gBinaryEncode{|[ !]|}   c xs             st = encodeList c xs st
gBinaryEncode{|[!!]|}   c xs             st = encodeList c xs st

encodeInt :: !Int !*EncodingSt -> *EncodingSt
encodeInt int st = encodeIntUsingNBytes (IF_INT_64_OR_32 8 4) int st

encodeChar :: !Char !*EncodingSt -> *EncodingSt
encodeChar c st = encodeIntUsingNBytes 1 (toInt c) st

encodeBool :: !Bool !*EncodingSt -> *EncodingSt
encodeBool False st = {st & es_pos = st.es_pos + 1}
encodeBool True cs=:{es_pos = pos, es_bits = bits}
	#! byte_pos = pos >> 3
	#! bit_pos = pos bitand 7
	#! int = toInt bits.[byte_pos]
	#! bit_mask = 1 << bit_pos
	= {cs & es_pos = inc pos, es_bits = {bits & [byte_pos] = toChar $ int bitor bit_mask}}

encodeReal :: !Real !*EncodingSt -> *EncodingSt
encodeReal real st = IF_INT_64_OR_32
	(encodeInt (unsafeCoerce real) st)
	(let (i1, i2) = realToInts real in encodeInt i2 $ encodeInt i1 st)
where
	realToInts :: !Real -> (!Int, !Int)
	realToInts _ = code {
		no_op
	}

encodeArray :: !(a *EncodingSt -> *EncodingSt) !(b a) !*EncodingSt -> *EncodingSt | Array b a
encodeArray f xs st
	#! st = encodeInt (size xs) st
	= foldlArr (flip f) st xs

encodeList :: !(a *EncodingSt -> *EncodingSt) !(l a) !*EncodingSt -> *EncodingSt | List l a
encodeList f xs st
	#! st = encodeInt (Length xs) st
	= Foldl (flip f) st xs

encodeIntUsingNBytes :: !Int !Int !*EncodingSt -> *EncodingSt
encodeIntUsingNBytes numBytes int st = encode numBytes $ withByteAlignedPosition st
where
	encode :: !Int !*EncodingSt -> *EncodingSt
	encode 0              st = st
	encode remainingBytes st
		#! byte_pos = st.es_pos >> 3
		#! st =
			{ st
			& es_bits = {st.es_bits & [byte_pos] = toChar $ int >> ((numBytes - remainingBytes) * 8)}
			, es_pos  = st.es_pos + 8
			}
		= encode (dec remainingBytes) st

encodeIntUsingNBits :: !Int !Int !*EncodingSt -> *EncodingSt
encodeIntUsingNBits 0 _ st = st
encodeIntUsingNBits numBits int st
	# st = encodeBool (int bitand 1 == 1) st
	= encodeIntUsingNBits (numBits - 1) (int >> 1) st

generic gBinaryEncodingSize a :: !a !Int -> Int
gBinaryEncodingSize{|Int|}    _                s  = (IF_INT_64_OR_32 64 32) + byteAlignedPosition s
gBinaryEncodingSize{|Real|}   _                s  = 64 + byteAlignedPosition s
gBinaryEncodingSize{|Char|}   _                s  = 8 + byteAlignedPosition s
gBinaryEncodingSize{|Bool|}   _                s = 1 + s
gBinaryEncodingSize{|String|} xs               s = IF_INT_64_OR_32 64 32 + size xs * 8 + byteAlignedPosition s
gBinaryEncodingSize{|UNIT|}   _                s = s
gBinaryEncodingSize{|PAIR|}   cx cy (PAIR x y) s = cy y $ cx x s
gBinaryEncodingSize{|EITHER|} cl _ (LEFT x)    s = cl x s
gBinaryEncodingSize{|EITHER|} _ cr (RIGHT x)   s = cr x s
gBinaryEncodingSize{|CONS of d|} c (CONS x)    s = c x $ ceil_log2 s d.gcd_type_def.gtd_num_conses
gBinaryEncodingSize{|FIELD|}  c (FIELD x)      s = c x s
gBinaryEncodingSize{|OBJECT|} c (OBJECT x)     s = c x s
gBinaryEncodingSize{|RECORD|} c (RECORD x)     s = c x s
gBinaryEncodingSize{|[]|}  c xs s   = foldl    (flip c) (IF_INT_64_OR_32 64 32 + byteAlignedPosition s) xs
gBinaryEncodingSize{|[!]|}  c xs s  = Foldl    (flip c) (IF_INT_64_OR_32 64 32 + byteAlignedPosition s) xs
gBinaryEncodingSize{|[ !]|}  c xs s = Foldl    (flip c) (IF_INT_64_OR_32 64 32 + byteAlignedPosition s) xs
gBinaryEncodingSize{|[!!]|}  c xs s = Foldl    (flip c) (IF_INT_64_OR_32 64 32 + byteAlignedPosition s) xs
gBinaryEncodingSize{|{}|}  c xs s   = foldlArr (flip c) (IF_INT_64_OR_32 64 32 + byteAlignedPosition s) xs
gBinaryEncodingSize{|{!}|} c xs s   = foldlArr (flip c) (IF_INT_64_OR_32 64 32 + byteAlignedPosition s) xs

generic gBinaryDecode a :: !*EncodingSt -> (!?a, !*EncodingSt)
gBinaryDecode{|Int|}    st = decodeInt st
gBinaryDecode{|Real|}   st = decodeReal st
gBinaryDecode{|Char|}   st = decodeChar st
gBinaryDecode{|Bool|}   st = decodeBool st
gBinaryDecode{|String|} st = decodeArray decodeChar st
gBinaryDecode{|UNIT|} st = (?Just UNIT, st)
gBinaryDecode{|PAIR|} fx fy st
	# (mbX, st) = fx st
	# (mbY, st) = fy st
	= case (mbX, mbY) of
		(?Just x, ?Just y) = (?Just $ PAIR x y, st)
		_                  = (?None,            st)
gBinaryDecode{|EITHER|} fl fr st = case st.es_cons_path of
	[]               = (?None, st)
	[ConsLeft:path]  = appFst (fmap LEFT)  $ fl {st & es_cons_path=path}
	[ConsRight:path] = appFst (fmap RIGHT) $ fr {st & es_cons_path=path}
gBinaryDecode{|CONS|}   f st = appFst (fmap CONS) $ f st
gBinaryDecode{|FIELD|}  f st = appFst (fmap \x -> FIELD x) $ f st
gBinaryDecode{|OBJECT of {gtd_conses,gtd_num_conses}|} f st =
	case decodeIntWithNBits (ceil_log2 0 gtd_num_conses) st of
		(?None, st)   = (?None, st)
		(?Just i, st) = appFst (fmap \x -> OBJECT x) $ f {st & es_cons_path=getConsPath (gtd_conses!!i)}
gBinaryDecode{|RECORD|} f st = appFst (fmap RECORD) $ f st
gBinaryDecode{|[]|}     f st = decodeList f st
gBinaryDecode{|[!]|}    f st = decodeList f st
gBinaryDecode{|[ !]|}   f st = decodeList f st
gBinaryDecode{|[!!]|}   f st = decodeList f st
gBinaryDecode{|{}|}     f st = decodeArray f st
gBinaryDecode{|{!}|}    f st = decodeArray f st

decodeInt :: !*EncodingSt -> (!?Int, !*EncodingSt)
decodeInt st = decodeIntWithNBytes (IF_INT_64_OR_32 8 4) st

decodeChar :: !*EncodingSt -> (!?Char, !*EncodingSt)
decodeChar st
	# (mbInt, st) = decodeIntWithNBytes 1 st
	= (toChar <$> mbInt, st)

decodeBool :: !*EncodingSt -> (!?Bool, !*EncodingSt)
decodeBool cs=:{es_pos = pos, es_bits = bits}
	#! s = size bits
	#! byte_pos = pos >> 3
	#! bit_pos = pos bitand 7
	| s == byte_pos = (?None, cs)
	#! int = toInt bits.[byte_pos]
	#! bit_mask = 1 << bit_pos
	#! bit = (bit_mask bitand int) <> 0
	= (?Just bit, {cs & es_pos = inc pos})

decodeReal :: !*EncodingSt -> (!?Real, !*EncodingSt)
decodeReal st = IF_INT_64_OR_32 decodeReal64 decodeReal32 $ st
where
	decodeReal64 st
		# (mbInt, st) = decodeInt st
		= (unsafeCoerce <$> mbInt, st)

	decodeReal32 st
		# (mbInt1, st) = decodeInt st
		# (mbInt2, st) = decodeInt st
		= case (mbInt1, mbInt2) of
			(?Just int1, ?Just int2) = (?Just $ intsToReal int1 int2, st)
			_                        = (?None, st)
	where
		intsToReal :: !Int !Int -> Real
		intsToReal _ _ = code {
			no_op
		}

decodeArray :: !(*EncodingSt -> (?a, *EncodingSt)) !*EncodingSt -> (!?(b a), !*EncodingSt) | Array b a
decodeArray f st
	# (mbLength, st) = decodeInt st
	= case mbLength of
		?Just l = decodeArray 0 l (unsafeCreateArray l) st
		_       = (?None, st)
where
	decodeArray i s arr st
		| i == s = (?Just arr, st)
		| otherwise
			# (mbX, st) = f st
			= case mbX of
				?Just x = decodeArray (inc i) s {arr & [i] = x} st
				_       = (?None, st)

decodeList :: !(*EncodingSt -> (?a, *EncodingSt)) !*EncodingSt -> (!?(l a), !*EncodingSt) | List l a
decodeList xs st
	# (mbArr, st) = decodeArray xs st
	= (arrToList <$> mbArr, st)
where
	arrToList :: !{b} -> l b | List l b
	arrToList xs = [|x \\ x <-: xs]

decodeIntWithNBytes :: !Int !*EncodingSt -> (!?Int, !*EncodingSt)
decodeIntWithNBytes numBytes st=:{es_pos} = decode numBytes 0 $ withByteAlignedPosition st
where
	// we can decode an entire byte at once, as the start position is byte-aligned
	decode :: !Int !Int !*EncodingSt -> (!?Int, !*EncodingSt)
	decode 0              int st = (?Just int, st)
	decode remainingBytes int st=:{es_bits}
		#! byte_pos = st.es_pos >> 3
		| byte_pos == size es_bits = (?None, st)
		#! byte = toInt es_bits.[byte_pos]
		= decode (dec remainingBytes) (byte << ((numBytes - remainingBytes) * 8) + int) {st & es_pos = st.es_pos + 8}

decodeIntWithNBits :: !Int !*EncodingSt -> (!?Int, !*EncodingSt)
decodeIntWithNBits numBits st = decode numBits 0 st
where
	decode :: !Int !Int !*EncodingSt -> (!?Int, !*EncodingSt)
	decode 0 int st = (?Just int, st)
	decode remainingBits int st
		# (mbBool,st) = decodeBool st
		| isNone mbBool
			= (?None, st)
			= decode (remainingBits - 1) (int + (if (fromJust mbBool) 1 0 << (numBits - remainingBits))) st

withByteAlignedPosition :: !*EncodingSt -> *EncodingSt
withByteAlignedPosition st=:{es_pos} = {st & es_pos = byteAlignedPosition es_pos}

byteAlignedPosition :: !Int -> Int
byteAlignedPosition pos = (pos + 7) bitand -8

ceil_log2 :: !Int !Int -> Int
ceil_log2 acc n
	| n < 2
		= acc
		= ceil_log2 (acc + 1) (n / 2 + n rem 2)

derive gBinaryEncode       (), (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)
derive gBinaryEncodingSize (), (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)
derive gBinaryDecode       (), (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)
