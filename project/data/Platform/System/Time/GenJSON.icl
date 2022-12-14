implementation module System.Time.GenJSON

import System.Time, Text.GenJSON

JSONEncode{|Timestamp|} _ (Timestamp t) = [JSONInt t]
JSONDecode{|Timestamp|} _ [JSONInt t:c] = (?Just (Timestamp t), c)
JSONDecode{|Timestamp|} _ c             = (?None, c)

derive JSONEncode Timespec, Tm, Clock
derive JSONDecode Timespec, Tm, Clock