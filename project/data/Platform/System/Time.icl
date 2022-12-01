implementation module System.Time

import StdEnv
import Data.GenEq
import System.OS
import System.OSError
import System._Pointer
import qualified System._Time
import Text

//String buffer size
MAXBUF :== 256

derive gEq Timestamp

instance == Timestamp
where
	(==) (Timestamp t1) (Timestamp t2) = t1 == t2

instance < Timestamp
where
	(<) (Timestamp t1) (Timestamp t2) = t1 < t2

instance toString Tm
where
	toString tm = trim (derefString (toStringTmC (packTm tm)))
	where
		toStringTmC :: !{#Int} -> Pointer
		toStringTmC a0 = code {
			ccall asctime "A:p"
		}
instance toString Timestamp
where
	toString (Timestamp t)
	| IF_WINDOWS (t < 0) False
		= abort "System.Time: Timestamp cannot be negative\n"
		= trim (derefString (toStringTimeC (packInt t)))
	where
		toStringTimeC :: !{#Int} -> Pointer
		toStringTimeC a0 = code {
			ccall ctime "A:p"
		}
instance toString Clock
where
	toString (Clock c) = toString c
instance toInt Timestamp
where
	toInt (Timestamp i) = i

clock :: !*World -> (!Clock, !*World)
clock world
	# (c, world) = IF_WINDOWS (clock_windows world) (clock_posix world)
	= (Clock c, world)
where
	clock_posix :: !*World -> (!Int, !*World)
	clock_posix world = code {
		ccall clock ":I:A"
	}
	clock_windows :: !*World -> (!Int, !*World)
	clock_windows world = code {
		ccall clock ":I:I"
	}

time :: !*World -> (!Timestamp, !*World)
time world
	# (t, world) = IF_WINDOWS (time_windows 0 world) (time_posix 0 world)
	= (Timestamp t, world)
where
	time_posix :: !Int !*World -> (!Int,!*World)
	time_posix a0 world = code {
		ccall time "I:I:A"
	}
	time_windows :: !Int !*World -> (!Int,!*World)
	time_windows a0 world = code {
		ccall time "I:I:I"
	}

gmTime :: !*World -> (!Tm, !*World)
gmTime world
	# ((Timestamp t),world)	= time world
	# tm					= gmTimeC (packInt t)
	= (derefTm tm, world)

localTime :: !*World -> (!Tm, !*World)
localTime world
	# ((Timestamp t),world)	= time world
	# (tm,world)			= localTimeC (packInt t) world
	= (derefTm tm, world)

mkTime :: !Tm !*World-> (!Timestamp, !*World)
mkTime tm world
	# (t, world) = IF_WINDOWS mkTime_windows mkTime_posix (packTm tm) world
	= (Timestamp t, world)
where
	mkTime_posix :: !{#Int} !*World -> (!Int, !*World)
	mkTime_posix tm world = code {
		ccall mktime "A:I:A"
	}
	mkTime_windows :: !{#Int} !*World -> (!Int, !*World)
	mkTime_windows tm world = code {
		ccall mktime "A:I:I"
	}

timeGm :: !Tm -> Timestamp
timeGm tm = Timestamp ('System._Time'._timegm (packTm tm))

diffTime :: !Timestamp !Timestamp -> Int
diffTime (Timestamp t1) (Timestamp t2) = t1 - t2

strfTime :: !String !Tm -> String
strfTime format tm
	# buf		= createArray MAXBUF 'X'
	# (len,buf)	= strfTimeC buf MAXBUF (packString format) (packTm tm) buf
	= buf % (0, len - 1)
	where
		strfTimeC :: !{#Char} !Int !{#Char} !{#Int} !{#Char} -> (!Int,!{#Char})
		strfTimeC a0 a1 a2 a3 a4 = code {
			ccall strftime "sIsA:I:A"
		}

toLocalTime :: !Timestamp !*World -> (!Tm,!*World)
toLocalTime (Timestamp t) world
	# (tm,world) = localTimeC (packInt t) world
	= (derefTm tm, world)

toGmTime :: !Timestamp -> Tm
toGmTime (Timestamp t) = derefTm (gmTimeC (packInt t))

gmTimeC :: !{#Int} -> Pointer
gmTimeC tm = code {
	ccall gmtime "A:p"
}

localTimeC tm world :== IF_WINDOWS (localTime_windows tm world) (localTime_posix tm world)
where
	localTime_posix :: !{#Int} !*World -> (!Pointer, !*World)
	localTime_posix tm world = code {
		ccall localtime "A:p:A"
	}
	localTime_windows :: !{#Int} !*World -> (!Pointer, !*World)
	localTime_windows tm world = code {
		ccall localtime "A:p:I"
	}

derefTm :: !Pointer -> Tm
derefTm tm =
	{ sec   = readInt4S tm 0
	, min   = readInt4S tm 4
	, hour  = readInt4S tm 8
	, mday  = readInt4S tm 12
	, mon   = readInt4S tm 16
	, year  = readInt4S tm 20
	, wday  = readInt4S tm 24
	, yday  = readInt4S tm 28
	, isdst = readInt4S tm 32
	}

packTm :: !Tm -> {#Int}
packTm tm = (IF_INT_64_OR_32 packTm64 packTm32) tm

packTm64 :: !Tm -> {#Int}
packTm64 tm =
	{ tm.sec  + tm.min  << 32
	, tm.hour + tm.mday << 32
	, tm.mon  + tm.year << 32
	, tm.wday + tm.yday << 32
	, tm.isdst
	// NB: we need to add padding for tm_gmtoff and tm_zone because the C
	// functions that take this struct as an argument may override it. Without
	// this padding, these functions may write into the next node in the Clean
	// heap causing heap correction and, eventually, segmentation faults.
	, 0 // tm_gmtoff
	, 0 // tm_zone
	}

packTm32 :: !Tm -> {#Int}
packTm32 tm =
	{ tm.sec
	, tm.min
	, tm.hour
	, tm.mday
	, tm.mon
	, tm.year
	, tm.wday
	, tm.yday
	, tm.isdst
	// NB: see packTm64 for why padding is needed.
	, 0 // tm_gmtoff
	, 0
	, 0 // tm_zone
	}

nsTime :: !*World -> (!Timespec, !*World)
nsTime w = 'System._Time'._nsTime w

timespecSleep :: !Timespec !*World -> (!MaybeOSError (), !*World)
timespecSleep ts w = 'System._Time'._tsSleep ts w

timespecToStamp :: !Timespec -> Timestamp
timespecToStamp t = Timestamp t.tv_sec

timestampToSpec :: !Timestamp -> Timespec
timestampToSpec (Timestamp t) = {tv_sec=t,tv_nsec=0}

instance < Timespec
where
	(<) t1 t2
		| t1.tv_sec == t2.tv_sec = t1.tv_nsec < t2.tv_nsec
		= t1.tv_sec < t2.tv_sec

instance + Timespec
where
	(+) t1 t2 = let tv_nsec = t1.tv_nsec + t2.tv_nsec in
		{ tv_sec  = t1.tv_sec + t2.tv_sec + tv_nsec / 1000000000
		, tv_nsec = tv_nsec rem 1000000000
		}

instance - Timespec
where
	(-) t1 t2
		# tv_nsec = t1.tv_nsec - t2.tv_nsec
		| tv_nsec < 0
			= {tv_sec = t1.tv_sec - t2.tv_sec - 1, tv_nsec = 1000000000 + tv_nsec}
			= {tv_sec = t1.tv_sec - t2.tv_sec,     tv_nsec = tv_nsec}

instance zero Timespec
where zero = {tv_sec=0, tv_nsec=0}

instance == Timespec
where
	(==) t1 t2 = t1.tv_sec == t2.tv_sec && t1.tv_nsec == t2.tv_nsec
