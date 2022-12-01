implementation module Data.Generics

import StdGeneric

class genericDescriptorName a :: !a -> String
instance genericDescriptorName GenericTypeDefDescriptor where genericDescriptorName gtd = gtd.gtd_name
instance genericDescriptorName GenericConsDescriptor where genericDescriptorName gcd = gcd.gcd_name
instance genericDescriptorName GenericRecordDescriptor where genericDescriptorName grd = grd.grd_name
instance genericDescriptorName GenericFieldDescriptor where genericDescriptorName gfd = gfd.gfd_name

fromOBJECT :: !(OBJECT x) -> x
fromOBJECT (OBJECT x) = x

fromCONS :: !(CONS x) -> x
fromCONS (CONS x) = x

fromRECORD :: !(RECORD x) -> x
fromRECORD (RECORD x) = x

fromFIELD :: !(FIELD x) -> x
fromFIELD (FIELD x) = x

fromPAIRX :: !(PAIR x y) -> x
fromPAIRX (PAIR x _) = x

fromPAIRY :: !(PAIR x y) -> y
fromPAIRY (PAIR _ y) = y
