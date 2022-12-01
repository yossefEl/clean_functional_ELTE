definition module System.Time.GenJSON

from System.Time  import :: Timestamp, :: Timespec, :: Tm, :: Clock
from Text.GenJSON import generic JSONEncode, generic JSONDecode, :: JSONNode

derive JSONEncode Timestamp, Timespec, Tm, Clock
derive JSONDecode Timestamp, Timespec, Tm, Clock