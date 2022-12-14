definition module GenCompress

import StdGeneric

:: BitVector :== {#Int}

:: CompressSt

generic gCompress a :: !a -> *CompressSt -> *CompressSt
derive gCompress Int, Real, Bool, Char, String, UNIT, PAIR, EITHER, CONS, FIELD, OBJECT, [], {}, {!}

generic gCompressedSize a :: a -> Int
derive gCompressedSize Int, Real, Bool, Char, String, UNIT, PAIR, EITHER, CONS, FIELD, OBJECT, [], {}, {!}

generic gUncompress a :: (u:CompressSt -> ((?a),u:CompressSt))
derive gUncompress Int, Real, Bool, Char, String, UNIT, PAIR, EITHER, CONS, FIELD, OBJECT, [], {}, {!}

compress :: !a -> BitVector | gCompressedSize{|*|} a & gCompress{|*|} a
uncompress :: (BitVector -> ?a) | gUncompress{|*|} a
