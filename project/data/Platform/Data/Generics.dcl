definition module Data.Generics

import StdGeneric

class genericDescriptorName a :: !a -> String
instance genericDescriptorName GenericTypeDefDescriptor, GenericConsDescriptor, GenericRecordDescriptor, GenericFieldDescriptor

fromOBJECT :: !(OBJECT x) -> x
fromCONS   :: !(CONS x)   -> x
fromRECORD :: !(RECORD x) -> x
fromFIELD  :: !(FIELD x)  -> x
fromPAIRX  :: !(PAIR x y) -> x
fromPAIRY  :: !(PAIR x y) -> y
