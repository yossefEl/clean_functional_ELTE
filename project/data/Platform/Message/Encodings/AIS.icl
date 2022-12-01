implementation module Message.Encodings.AIS

import StdEnv
import Data.Func, Data.GenDefault, Data.Tuple, Data.Maybe.GenDefault
import Text

decodeAIVDM :: ![String] -> (![AIVDM], ![String])
decodeAIVDM [] = ([],[])
decodeAIVDM sentences
	# (payload, remainder) = decodeWrapper sentences
	= case payload of
		[]  = ([],remainder)
		parts
			# cnb = decodePayload (concat parts)
			# (cnbs,remainder) = decodeAIVDM remainder
			= ([cnb:cnbs],remainder)
where
	//Take a number of fragments from the stream to unwrap and reconstruct the message payload
	decodeWrapper :: [String] -> ([{#Char}], [String])
	decodeWrapper [] = ([],[])
	decodeWrapper [sentence:remainder]
		= case split "," sentence of
			[f1,f2,f3,f4,f5,f6,f7]
				| f1 <> "!AIVDM" && f1 <> "!BSVDM" && f1 <> "!ABVDM" && f1 <> "!AIVDO" && f1 <> "!BSVDO" &&
				  f1 <> "!BYVDM" && f1 <> "!SSVDM" && f1 <> "!SSVDO" && f1 <> "!BYVDO" && f1 <> "!ABVDO"
					= ([],remainder)
				# fragmentCount = toInt f2
				# fragmentNum = toInt f3
				| fragmentCount == fragmentNum
					= ([f6],remainder)
				| otherwise
					# (fragments,remainder) = decodeWrapper remainder
					= ([f6:fragments],remainder)
			_ 	= ([],remainder)

	//Decode the message data
	decodePayload :: {#Char} -> AIVDM
	decodePayload bits
		# bits = bv_unarmor bits
		= snd ((decodeDyn decodeMsgType decoderByType) bits 0)
	where
		decodeMsgType bits pos
			# (pos, m) = decodeUnsigned 6 (\t _ -> initMessage t) bits pos (AIVDM 0)
			= case m of
				AIVDM24 _
					# (pos, repeat) = decodeUnsigned  2 (\repeat _ -> repeat) bits pos 0
					# (pos, mmsi)   = decodeUnsigned 30 (\mmsi   _ -> mmsi)   bits pos 0
					# (pos, partNr) = decodeUnsigned  2 (\partNr _ -> partNr) bits pos 0
					| partNr == 0 =
						(pos, AIVDM24 $ AIVDM24A {AIVDM24A| msgtype = 24, repeat = repeat, mmsi = mmsi, shipname = ""})
					| otherwise =
						(pos, AIVDM24 $ AIVDM24B {AIVDM24B| gDefault{|*|} & repeat = repeat, mmsi = mmsi})
				_
					= (pos, m)

		decoderByType :: !{#Char} !Int !AIVDM -> (!Int, !AIVDM)
		decoderByType bits pos msg = case msg of
			AIVDM1  m = appSnd AIVDM1  $ decodeCNB bits pos m
			AIVDM2  m = appSnd AIVDM2  $ decodeCNB bits pos m
			AIVDM3  m = appSnd AIVDM3  $ decodeCNB bits pos m
			AIVDM4  m = appSnd AIVDM4  $ decodeType4Or11 bits pos m
			AIVDM5  m = appSnd AIVDM5  $ decodeType5     bits pos m
			AIVDM9  m = appSnd AIVDM9  $ decodeType9     bits pos m
			AIVDM11 m = appSnd AIVDM11 $ decodeType4Or11 bits pos m
			AIVDM18 m = appSnd AIVDM18 $ decodeType18    bits pos m
			AIVDM19 m = appSnd AIVDM19 $ decodeType19    bits pos m
			AIVDM21 m = appSnd AIVDM21 $ decodeType21    bits pos m
			AIVDM24 m =
				case m of
					AIVDM24A m -> appSnd (AIVDM24 o AIVDM24A) $ decodeType24A bits pos m
					AIVDM24B m -> appSnd (AIVDM24 o AIVDM24B) $ decodeType24B bits pos m
			AIVDM27 m = appSnd AIVDM27 $ decodeType27    bits pos m
			m         = decodeSpare 0 bits pos m//Nop

		initMessage 1  = AIVDM1  (initCNB 1)
		initMessage 2  = AIVDM2  (initCNB 2)
		initMessage 3  = AIVDM3  (initCNB 3)
		initMessage 4  = AIVDM4  (initType4Or11 4)
		initMessage 5  = AIVDM5  initType5
		initMessage 9  = AIVDM9  gDefault{|*|}
		initMessage 11 = AIVDM11 (initType4Or11 11)
		initMessage 18 = AIVDM18 gDefault{|*|}
		initMessage 19 = AIVDM19 gDefault{|*|}
		initMessage 21 = AIVDM21 gDefault{|*|}
		initMessage 24 = AIVDM24 gDefault{|*|}
		initMessage 27 = AIVDM27 gDefault{|*|}
		initMessage  i = AIVDM i

		initCNB type = {msgtype=type,repeat=0,mmsi=0,status=0,turn= ?None,speed= ?None,accuracy=False
					   ,lon=0,lat=0,course= ?None,heading= ?None,second=0,maneuver=0,raim=False,radio=0}
		initType4Or11 type =
				{msgtype=type,repeat=0,mmsi=0,year=0,month=0,day=0,hour=0,minute=0
				,second=0,accuracy=False,lon=0,lat=0,epfd=0,raim=False,radio=0}
		initType5    = {AIVDM5| msgtype=5,repeat=0,mmsi=0,ais_version=0,imo_id=0,callsign="",shipname=""
					   ,shiptype=0,to_bow=0,to_stern=0,to_port=0,to_starboard=0,epfd=0,month=0
					   ,day=0,hour=0,minute=0,draught=0,destination="",dte=False}

		decodeCNB = decodeMultiple
			[decodeUnsigned  2 (\i m -> {AIVDMCNB| m & repeat = i})
			,decodeUnsigned 30 (\i m -> {AIVDMCNB| m & mmsi = i})
			,decodeUnsigned 4 (\i cnb -> {AIVDMCNB|cnb & status = i})
			,decodeSigned 8 (updCnbIf ((<>) -128) (\i cnb -> {AIVDMCNB|cnb & turn = ?Just i}))
			,decodeUnsigned 10 (updCnbIf ((<>) 1023) (\i cnb -> {AIVDMCNB|cnb & speed = ?Just i}))
			,decodeUnsigned 1 (\i cnb -> {AIVDMCNB|cnb & accuracy = i > 0})
			,decodeSigned 28 (\i cnb -> {AIVDMCNB|cnb & lon = i})
			,decodeSigned 27 (\i cnb -> {AIVDMCNB|cnb & lat = i})
			,decodeUnsigned 12 (updCnbIf ((<>) 3600) (\i cnb -> {AIVDMCNB|cnb & course = ?Just i}))
			,decodeUnsigned 9 (updCnbIf ((<>) 511) (\i cnb -> {AIVDMCNB|cnb & heading = ?Just i}))
			,decodeUnsigned 6 (\i cnb -> {AIVDMCNB|cnb & second = i})
			,decodeUnsigned 2 (\i cnb -> {AIVDMCNB|cnb & maneuver = i})
			,decodeSpare 3
			,decodeUnsigned 1 (\i cnb -> {AIVDMCNB|cnb & raim = i > 0})
			,decodeUnsigned 19 (\i cnb -> {AIVDMCNB|cnb & radio = i})
			]

		decodeType4Or11 = decodeMultiple
			[ decodeUnsigned  2 (\i m -> {AIVDM4Or11| m & repeat = i})
			, decodeUnsigned 30 (\i m -> {AIVDM4Or11| m & mmsi = i})
			, decodeUnsigned 14 (\i m -> {AIVDM4Or11| m & year = i})
			, decodeUnsigned  4 (\i m -> {AIVDM4Or11| m & month = i})
			, decodeUnsigned  5 (\i m -> {AIVDM4Or11| m & day = i})
			, decodeUnsigned  5 (\i m -> {AIVDM4Or11| m & hour = i})
			, decodeUnsigned  6 (\i m -> {AIVDM4Or11| m & minute = i})
			, decodeUnsigned  6 (\i m -> {AIVDM4Or11| m & second = i})
			, decodeUnsigned  1 (\i m -> {AIVDM4Or11| m & accuracy = i > 0})
			, decodeSigned   28 (\i m -> {AIVDM4Or11| m & lon = i})
			, decodeSigned   27 (\i m -> {AIVDM4Or11| m & lat = i})
			, decodeUnsigned  4 (\i m -> {AIVDM4Or11| m & epfd = i})
			, decodeSpare    10
			, decodeUnsigned  1 (\i m -> {AIVDM4Or11| m & raim = i > 0})
			, decodeUnsigned 19 (\i m -> {AIVDM4Or11| m & radio = i})
			]

		decodeType5 = decodeMultiple
			[decodeUnsigned  2 (\i m -> {AIVDM5| m & repeat = i})
			,decodeUnsigned 30 (\i m -> {AIVDM5| m & mmsi = i})
			,decodeUnsigned 2 (\i m -> {AIVDM5|m & ais_version = i})
			,decodeUnsigned 30 (\i m -> {AIVDM5|m & imo_id = i})
			,decodeString 42 (\s m -> {AIVDM5|m & callsign = s})
			,decodeString 120 (\s m -> {AIVDM5|m & shipname= s})
			,decodeUnsigned 8 (\i m -> {AIVDM5|m & shiptype = i})
			,decodeUnsigned 9 (\i m -> {AIVDM5|m & to_bow = i})
			,decodeUnsigned 9 (\i m -> {AIVDM5|m & to_stern = i})
			,decodeUnsigned 6 (\i m -> {AIVDM5|m & to_port = i})
			,decodeUnsigned 6 (\i m -> {AIVDM5|m & to_starboard= i})
			,decodeUnsigned 4 (\i m -> {AIVDM5|m & epfd = i})
			,decodeUnsigned 4 (\i m -> {AIVDM5|m & month = i})
			,decodeUnsigned 5 (\i m -> {AIVDM5|m & day = i})
			,decodeUnsigned 5 (\i m -> {AIVDM5|m & hour = i})
			,decodeUnsigned 6 (\i m -> {AIVDM5|m & minute = i})
			,decodeUnsigned 8 (\i m -> {AIVDM5|m & draught = i})
			,decodeString 120 (\s m -> {AIVDM5|m & destination = s})
			,decodeUnsigned 1 (\i m -> {AIVDM5|m & dte = i > 0})
			,decodeSpare 1
			]

		decodeType9 = decodeMultiple
			[ decodeUnsigned  2 (\i m -> {AIVDM9| m & repeat = i})
			, decodeUnsigned 30 (\i m -> {AIVDM9| m & mmsi = i})
			, decodeUnsigned 12 (\i m -> if (i == 4095) m {AIVDM9| m & altitude = ?Just i})
			, decodeUnsigned 10 (\i m -> if (i == 1023) m {AIVDM9|m & speed = ?Just i})
			, decodeUnsigned  1 (\i m -> {AIVDM9|m & accuracy = i > 0})
			, decodeSigned   28 (\i m -> {AIVDM9|m & lon = i})
			, decodeSigned   27 (\i m -> {AIVDM9|m & lat = i})
			, decodeUnsigned 12 (\i m -> if (i == 3600) m {AIVDM9|m & course = ?Just i})
			, decodeUnsigned  6 (\i m -> {AIVDM9|m & second = i})
			, decodeSpare 8
			, decodeUnsigned  1 (\i m -> {AIVDM9| m & dte = i > 0})
			, decodeSpare 3
			, decodeUnsigned  1 (\i m -> {AIVDM9| m & assignedMode = i > 0})
			, decodeUnsigned  1 (\i m -> {AIVDM9|m & raim = i > 0})
			, decodeUnsigned 20 (\i m -> {AIVDM9|m & radio = i})
			]

		decodeType18 = decodeMultiple
			[ decodeUnsigned  2 (\i m -> {AIVDM18| m & repeat = i})
			, decodeUnsigned 30 (\i m -> {AIVDM18| m & mmsi = i})
			, decodeSpare     8
			, decodeUnsigned 10 (\i m -> {AIVDM18| m & speed = if (i == 1023) ?None (?Just i)})
			, decodeUnsigned  1 (\i m -> {AIVDM18| m & accuracy = i > 0})
			, decodeSigned   28 (\i m -> {AIVDM18| m & lon = i})
			, decodeSigned   27 (\i m -> {AIVDM18| m & lat = i})
			, decodeUnsigned 12 (\i m -> {AIVDM18| m & course = if (i == 3600) ?None (?Just i)})
			, decodeUnsigned  9 (\i m -> {AIVDM18| m & heading = if (i == 511) ?None (?Just i)})
			, decodeUnsigned  6 (\i m -> {AIVDM18| m & second = i})
			, decodeSpare     8
			, decodeUnsigned  1 (\i m -> {AIVDM18| m & raim = i > 0})
			, decodeUnsigned 20 (\i m -> {AIVDM18| m & radio = i})
			]

		decodeType19 = decodeMultiple
			[ decodeUnsigned  2 (\i m -> {AIVDM19| m & repeat = i})
			, decodeUnsigned 30 (\i m -> {AIVDM19| m & mmsi = i})
			, decodeSpare     8
			, decodeUnsigned 10 (\i m -> {AIVDM19| m & speed = if (i == 1023) ?None (?Just i)})
			, decodeUnsigned  1 (\i m -> {AIVDM19| m & accuracy = i > 0})
			, decodeSigned   28 (\i m -> {AIVDM19| m & lon = i})
			, decodeSigned   27 (\i m -> {AIVDM19| m & lat = i})
			, decodeUnsigned 12 (\i m -> {AIVDM19| m & course = if (i == 3600) ?None (?Just i)})
			, decodeUnsigned  9 (\i m -> {AIVDM19| m & heading = if (i == 511) ?None (?Just i)})
			, decodeUnsigned  6 (\i m -> {AIVDM19| m & second = i})
			, decodeSpare     4
			, decodeString  120 (\s m -> {AIVDM19| m & shipname= s})
			, decodeUnsigned  8 (\i m -> {AIVDM19| m & shiptype = i})
			, decodeUnsigned  9 (\i m -> {AIVDM19| m & to_bow = i})
			, decodeUnsigned  9 (\i m -> {AIVDM19| m & to_stern = i})
			, decodeUnsigned  6 (\i m -> {AIVDM19| m & to_port = i})
			, decodeUnsigned  6 (\i m -> {AIVDM19| m & to_starboard= i})
			, decodeUnsigned  4 (\i m -> {AIVDM19| m & epfd = i})
			, decodeUnsigned  1 (\i m -> {AIVDM19| m & raim = i > 0})
			, decodeUnsigned  1 (\i m -> {AIVDM19| m & dte = i > 0})
			, decodeUnsigned  1 (\i m -> {AIVDM19| m & assignedMode = i > 0})
			, decodeSpare     4
			]

		decodeType21 = decodeMultiple
			[ decodeUnsigned  2 (\i m -> {AIVDM21| m & repeat = i})
			, decodeUnsigned 30 (\i m -> {AIVDM21| m & mmsi = i})
			, decodeUnsigned  5 (\i m -> {AIVDM21| m & aid_type = i})
			, decodeString  120 (\s m -> {AIVDM21| m & name= s})
			, decodeUnsigned  1 (\i m -> {AIVDM21| m & accuracy = i > 0})
			, decodeSigned   28 (\i m -> {AIVDM21| m & lon = i})
			, decodeSigned   27 (\i m -> {AIVDM21| m & lat = i})
			, decodeUnsigned  9 (\i m -> {AIVDM21| m & to_bow = i})
			, decodeUnsigned  9 (\i m -> {AIVDM21| m & to_stern = i})
			, decodeUnsigned  6 (\i m -> {AIVDM21| m & to_port = i})
			, decodeUnsigned  6 (\i m -> {AIVDM21| m & to_starboard= i})
			, decodeUnsigned  4 (\i m -> {AIVDM21| m & epfd = i})
			, decodeUnsigned  6 (\i m -> {AIVDM21| m & second = i})
			, decodeUnsigned  1 (\i m -> if (m.AIVDM21.second <= 59) {AIVDM21| m & off_position = ?Just $ i > 0} m)
			, decodeSpare     8
			, decodeUnsigned  1 (\i m -> {AIVDM21| m & raim = i > 0})
			, decodeUnsigned  1 (\i m -> {AIVDM21| m & virtual_aid = i > 0})
			, decodeUnsigned  1 (\i m -> {AIVDM21| m & assignedMode = i > 0})
			, decodeSpare     1
			, decodeString   88 (\s m -> {AIVDM21| m & name = m.AIVDM21.name +++ s})
			]

		decodeType24A = decodeMultiple
			[ decodeString 120 (\n m -> {AIVDM24A| m & shipname = n})
			]

		decodeType24B = decodeMultiple
			[decodeUnsigned  8 (\i m -> {AIVDM24B|m & shiptype = i})
			,decodeString   42 (\s m -> {AIVDM24B|m & vendorid = s})
			,decodeString   42 (\s m -> {AIVDM24B|m & callsign = s})
			,decodeUnsigned  9 (\i m -> {AIVDM24B|m & to_bow = i})
			,decodeUnsigned  9 (\i m -> {AIVDM24B|m & to_stern = i})
			,decodeUnsigned  6 (\i m -> {AIVDM24B|m & to_port = i})
			,decodeUnsigned  6 (\i m -> {AIVDM24B|m & to_starboard= i})
			,decodeUnsigned  4 (\i m -> {AIVDM24B|m & epfd = i})
			,decodeSpare     2
			]

		decodeType27 = decodeMultiple
			[ decodeUnsigned  2 (\i m -> {AIVDM27| m & repeat = i})
			, decodeUnsigned 30 (\i m -> {AIVDM27| m & mmsi = i})
			, decodeUnsigned  1 (\i m -> {AIVDM27| m & accuracy = i > 0})
			, decodeUnsigned  1 (\i m -> {AIVDM27| m & raim = i > 0})
			, decodeUnsigned  4 (\i m -> {AIVDM27| m & status = i})
			, decodeSigned   18 (\i m -> {AIVDM27| m & lon = i * 1000})
			, decodeSigned   17 (\i m -> {AIVDM27| m & lat = i * 1000})
			, decodeUnsigned  6 (\i m -> {AIVDM27| m & speed = if (i == 63) ?None (?Just $ i * 10)})
			, decodeUnsigned  9 (\i m -> {AIVDM27| m & course = if (i == 511) ?None (?Just $ i * 10)})
			, decodeUnsigned  1 (\i m -> {AIVDM27| m & gnss = i == 0})
			, decodeSpare     1
			]

		updCnbIf updateFor f i m
			| updateFor i = f i m
			| otherwise   = m

	decodeUnsigned :: !Int !(Int msg -> msg) !{#Char} !Int !msg -> (!Int, !msg)
	decodeUnsigned width modifier bits pos msg
		= (pos + width, modifier (bv_ubits pos width bits) msg)

	decodeSigned :: !Int !(Int msg -> msg) !{#Char} !Int !msg -> (!Int, !msg)
	decodeSigned width modifier bits pos msg
		= (pos + width, modifier (bv_sbits pos width bits) msg)

	decodeString :: !Int !(String msg -> msg) !{#Char} !Int !msg -> (!Int, !msg)
	decodeString width modifier bits pos msg
		 = (pos + width, modifier chars msg)
	where
		chars = rtrim {char c \\ c <- takeWhile (\i -> i > 0) [bv_ubits (pos + i * 6) 6 bits \\ i <- [0 .. (width / 6) - 1]]}
		char i = if (i == 0 || i > 64) ' ' "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^- !\"#$%&'()*+,-./0123456789:;<=>?".[i]

	decodeSpare :: !Int !{#Char} !Int !msg -> (!Int, !msg)
	decodeSpare width bits pos msg = (pos + width, msg)

	decodeDyn ::
		!({#Char} Int -> (Int, msg)) !({#Char} Int msg -> (Int, msg)) !{#Char} !Int -> (!Int, !msg)
	decodeDyn first cont bits pos
		# (pos, msg) = first bits pos
		= cont bits pos msg

	decodeMultiple :: ![{#Char} Int msg -> (Int, msg)] !{#Char} !Int !msg -> (!Int, !msg)
	decodeMultiple [] bits pos msg = (pos,msg)
	decodeMultiple [f:fs] bits pos msg
		# (pos,msg) = f bits pos msg
		= decodeMultiple fs bits pos msg

//Lowlevel bit manipulation
bv_unarmor :: {#Char} -> {#Char}
bv_unarmor src = unarmor_and_copy src 0 0 0 0 (createArray required_bytes '\0')
where
	//The number of 8-bit bytes needed to hold all the bits
	num_bits = size src * 6
	required_bytes = num_bits / 8 + (if (num_bits rem 8 > 0) 1 0)

	unarmor_and_copy :: {#Char} Int Int Int Int *{#Char} -> *{#Char}
	unarmor_and_copy src srci desti buf bufwidth dest
		| srci >= size src
			| bufwidth > 0	//Add remaining buffer bits to dest
				# byte	= toChar (buf bitand ((2 ^ bufwidth) - 1))
				= {dest & [desti] = byte}
			| otherwise
				= dest //Done.
		# buf = (buf << 6) bitor (unarmor_char src.[srci])
		# bufwidth = bufwidth + 6
		| bufwidth >= 8	//Copy another byte to the destination array
			# byte		= toChar (buf >> (bufwidth - 8))
			# bufwidth	= bufwidth - 8
			= (unarmor_and_copy src (srci + 1) (desti + 1) buf bufwidth {dest & [desti] = byte})
		| otherwise
			= unarmor_and_copy src (srci + 1) desti buf bufwidth dest
	where
		//Extract the six bits from an armored ASCII character
		unarmor_char :: Char -> Int
		unarmor_char char = let bits = toInt char - 48 in (if (bits > 40) (bits - 8) bits)

bv_ubits :: Int Int {#Char} -> Int
bv_ubits start width bytes
    | start + width > (size bytes * 8) = -1 //abort ("NOT ENOUGH BYTES: " +++ toString (start + width - (size bytes * 8)))
	# field = copy_bytes (start / 8) ((start + width - 1) / 8) 0	//Copy all bytes that contain the bitfield
	# align = (start + width) rem 8
	# field = if (align > 0) (field >> (8 - align)) field			//Align the field
	# field = field bitand ((2 ^ width) - 1)						//Mask the field
	= field
where
	copy_bytes firstbyte lastbyte field
		| firstbyte > lastbyte	= field
		| otherwise				= copy_bytes (firstbyte + 1) lastbyte ((field << 8) bitor (toInt bytes.[firstbyte]))

bv_sbits :: Int Int {#Char} -> Int
bv_sbits start width bits
	# field = bv_ubits start width bits
	| field bitand (1 << (width - 1)) == 0	= field
											= ~(2 ^ width - field)

derive gDefault AIVDM9, AIVDM18, AIVDM19, AIVDM24, AIVDM24A, AIVDM24B, AIVDM21, AIVDM27
