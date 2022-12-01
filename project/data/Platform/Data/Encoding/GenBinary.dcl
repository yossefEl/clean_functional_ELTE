definition module Data.Encoding.GenBinary

/**
 * This module provides a compact binary encoding for arbitrary values.
 * The encoding is provided as character array.
 * Choices of ADTs are represented by a single bit.
 * Values of basic types (except `Bool`), arrays and lists are stored byte-aligned, which wastes only little space,
 * but significantly improves encoding and decoding time.
 *
 * @property-bootstrap
 *     import StdEnv, StdOverloadedList , Data.List, Data.Maybe, Data.Maybe.Gast, Data.Maybe.GenPrint
 *            , Data.Maybe.GenBinary
 *
 *     :: ADT = A String | B ADT | C ADT ADT | D ADT ADT ADT
 *
 *     derive gEq             ADT
 *     derive class GenBinary ADT
 *     derive class Gast      ADT
 *
 *     instance == ADT where
 *         (==) x y = x === y
 *
 *     :: Record = {a :: ADT, b :: ADT , c :: ADT}
 *
 *     derive gEq             Record
 *     derive class GenBinary Record
 *     derive class Gast      Record
 *
 *     instance == Record where
 *         (==) x y = x === y
 *
 * @property-test-with a = ?Bool
 * @property-test-with a = Int
 * @property-test-with a = String
 * @property-test-with a = Char
 * @property-test-with a = Real
 * @property-test-with a = (Int, Int)
 * @property-test-with a = (String, String)
 * @property-test-with a = (String, Int)
 * @property-test-with a = (Int, String)
 * @property-test-with a = [Bool]
 * @property-test-with a = [Int]
 * @property-test-with a = [String]
 * @property-test-with a = [Char]
 * @property-test-with a = [Real]
 * @property-test-with a = [!Int]
 * @property-test-with a = [Int!]
 * @property-test-with a = [!Int!]
 * @property-test-with a = ADT
 * @property-test-with a = Record
 */

from StdGeneric   import :: UNIT (..), :: PAIR (..), :: EITHER (..), :: CONS (..), :: OBJECT (..), :: RECORD (..),
                         :: FIELD (..),
                         :: GenericConsDescriptor{gcd_index,gcd_type_def},
                         :: GenericTypeDefDescriptor{gtd_conses,gtd_num_conses},
                         :: ConsPos(..), getConsPath
from StdInt       import class + (+), instance + Int
from StdList      import !!
from Data.Maybe   import instance Functor ?
from Data.Func    import $
from Data.Functor import class Functor (fmap)
from Data.Tuple   import appFst

/**
 * Encodes a values as character array.
 *
 * @param The value.
 * @result The encoded value.
 */
encode :: !a -> {#Char} | gBinaryEncodingSize{|*|}, gBinaryEncode{|*|} a

/**
 * Decodes a value.
 *
 * @param The value encoded as character array.
 * @result The corresponding value, if the provided array is a valid representation of a value.
 *
 * @property correctness: A.a :: a:
 *     // The `a == a` check is required as NaN Real values do not equal themselves.
 *     a == a ==> decode (encode a) =.= ?Just a
 */
decode :: !{#Char} -> ?a | gBinaryDecode{|*|} a

class GenBinary a | gBinaryEncode{|*|}, gBinaryEncodingSize{|*|}, gBinaryDecode{|*|} a

:: *EncodingSt = {es_pos :: !Int, es_bits :: !*{#Char}, es_cons_path :: ![ConsPos]}

generic gBinaryEncode a :: !a !*EncodingSt -> *EncodingSt
gBinaryEncode{|UNIT|} _ st = st
gBinaryEncode{|PAIR|} cx cy (PAIR x y) st = cy y $ cx x st
gBinaryEncode{|EITHER|} cl cr (LEFT x)   st = cl x st
gBinaryEncode{|EITHER|} cl cr (RIGHT x)  st = cr x st
gBinaryEncode{|CONS of d|} c (CONS x)    st = c x $ encodeIntUsingNBits (ceil_log2 0 d.gcd_type_def.gtd_num_conses) d.gcd_index st
gBinaryEncode{|FIELD|} c (FIELD x) st = c x st
gBinaryEncode{|OBJECT|} c (OBJECT x) st = c x st
gBinaryEncode{|RECORD|} c (RECORD x) st = c x st

derive gBinaryEncode Int, Real, Bool, Char, String, [], [!], [ !], [!!], {}, {!}, (), (,), (,,), (,,,), (,,,,), (,,,,,),
                     (,,,,,,), (,,,,,,,)

// Only exported for gBinaryEncode{|CONS|}
encodeIntUsingNBits :: !Int !Int !*EncodingSt -> *EncodingSt

generic gBinaryEncodingSize a :: !a !Int -> Int
gBinaryEncodingSize{|UNIT|} _ s = s
gBinaryEncodingSize{|PAIR|} cx cy (PAIR x y) s = cy y $ cx x s
gBinaryEncodingSize{|EITHER|} cl _ (LEFT x) s = cl x s
gBinaryEncodingSize{|EITHER|} _ cr (RIGHT x) s = cr x s
gBinaryEncodingSize{|CONS of d|} c (CONS x)    s = c x $ ceil_log2 s d.gcd_type_def.gtd_num_conses
gBinaryEncodingSize{|FIELD|} c (FIELD x) s = c x s
gBinaryEncodingSize{|OBJECT|} c (OBJECT x) s = c x s
gBinaryEncodingSize{|RECORD|} c (RECORD x) s = c x s

derive gBinaryEncodingSize Int, Real, Bool, Char, String, [], [!], [ !], [!!], {}, {!}, (), (,), (,,), (,,,), (,,,,),
                           (,,,,,), (,,,,,,), (,,,,,,,)

generic gBinaryDecode a :: !*EncodingSt -> (!?a, !*EncodingSt)
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
gBinaryDecode{|CONS|} f st = appFst (fmap CONS) $ f st
gBinaryDecode{|FIELD|} f st = appFst (fmap \x -> FIELD x) $ f st
gBinaryDecode{|OBJECT of {gtd_conses,gtd_num_conses}|} f st =
	case decodeIntWithNBits (ceil_log2 0 gtd_num_conses) st of
		(?None, st)   = (?None, st)
		(?Just i, st) = appFst (fmap \x -> OBJECT x) $ f {st & es_cons_path=getConsPath (gtd_conses!!i)}
gBinaryDecode{|RECORD|} f st = appFst (fmap RECORD) $ f st

derive gBinaryDecode Int, Real, Bool, Char, String, [], [!], [ !], [!!], {}, {!}, (), (,), (,,), (,,,), (,,,,), (,,,,,),
                     (,,,,,,), (,,,,,,,)

// This is only exported because it is used in exposed generic definitions.
decodeBool :: !*EncodingSt -> (!?Bool, !*EncodingSt)
encodeBool :: !Bool !*EncodingSt -> *EncodingSt
decodeIntWithNBits :: !Int !*EncodingSt -> (!?Int, !*EncodingSt)
ceil_log2 :: !Int !Int -> Int
