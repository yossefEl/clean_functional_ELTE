implementation module Data.Monoid

from StdOverloaded import class + (..), class * (..), class zero (..), class one (..)
from StdBool import &&, ||
from StdFunc import o, id
from StdList import ++


instance Semigroup ()
where
	mappend _ _  = ()

instance Monoid ()
where
	mempty = ()

instance Semigroup (Dual a) | Semigroup a
where
	mappend :: !(Dual a) !(Dual a) -> Dual a | Semigroup a
	mappend (Dual x) (Dual y) = Dual (mappend y x)

instance Monoid (Dual a) | Monoid a
where
	mempty = Dual mempty

instance Semigroup (Endo .a)
where
	mappend :: !(Endo .a) !(Endo .a) -> Endo .a
	mappend (Endo f) (Endo g) = Endo (f o g)

instance Monoid (Endo .a)
where
	mempty = Endo id

instance Semigroup All
where
	mappend :: !All !All -> All
	mappend (All x) (All y) = All (x && y)

instance Monoid All
where
	mempty = All True

instance Semigroup Any
where
	mappend :: !Any !Any -> Any
	mappend (Any x) (Any y) = Any (x || y)

instance Monoid Any
where
	mempty = Any False

instance Semigroup (Sum a) | +, zero a
where
	mappend :: !(Sum a) !(Sum a) -> Sum a | +, zero a
	mappend (Sum x) (Sum y) = Sum (x + y)

instance Monoid (Sum a) | + a & zero a
where
	mempty = Sum zero

instance Semigroup (Product a) | *, one a
where
	mappend :: !(Product a) !(Product a) -> Product a | *, one a
	mappend (Product x) (Product y) = Product (x * y)

instance Monoid (Product a) | * a & one a
where
	mempty = Product one

instance Semigroup (First a)
where
	mappend r=:(First (?Just _)) _ = r
	mappend (First ?None) r = r

instance Monoid (First a)
where
	mempty = First ?None

instance Semigroup (Last a)
where
	mappend :: !(Last a) !(Last a) -> Last a
	mappend _ r=:(Last (?Just _)) = r
	mappend r (Last ?None) = r

instance Monoid (Last a)
where
	mempty = Last ?None

getDual :: !(Dual .a) -> .a
getDual (Dual x) = x

appEndo :: !(Endo .a) -> (.a -> .a)
appEndo (Endo f) = f

getAll :: !All -> Bool
getAll (All b) = b

getAny :: !Any -> Bool
getAny (Any b) = b

getSum :: !(Sum a) -> a
getSum (Sum x) = x

getProduct :: !(Product a) -> a
getProduct (Product x) = x

getFirst :: !(First a) -> ?a
getFirst (First x) = x

getLast :: !(Last a) -> ?a
getLast (Last x) = x
