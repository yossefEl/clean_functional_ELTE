module freevars

import StdGeneric, StdEnv

:: Expr 	= EApp Expr Expr 
			| ELambda Var Expr
			| EVar Var
:: Var 		= Var Int
instance == Var where 
	(==) (Var x) (Var y) = x == y
			
generic gCollectFreeVars a :: a -> [Var]
gCollectFreeVars {|Var|} v					= [v]
gCollectFreeVars {|Int|} x					= []
gCollectFreeVars {|UNIT|} UNIT				= []
gCollectFreeVars {|PAIR|} cx cy (PAIR x y)	= removeDup (cx x ++ cy y)
gCollectFreeVars {|EITHER|} cl cr (LEFT x)	= cl x
gCollectFreeVars {|EITHER|} cl cr (RIGHT x)	= cr x
gCollectFreeVars {|CONS|} f (CONS x) 		= f x
gCollectFreeVars {|FIELD|} f (FIELD x) 		= f x
gCollectFreeVars {|OBJECT|} f (OBJECT x) 	= f x
gCollectFreeVars {|Expr|} (ELambda v x) 	= filter ((<>) v) (gCollectFreeVars{|*|} x)
gCollectFreeVars {|Expr|} (EApp x y) 		= removeDup (gCollectFreeVars{|*|} x ++ gCollectFreeVars{|*|} y)
gCollectFreeVars {|Expr|} (EVar x) 			= gCollectFreeVars{|*|} x

collectFreeVars = gCollectFreeVars{|*|} 

derive bimap []
derive gCollectFreeVars []

Start = collectFreeVars 
	[ ELambda (Var 1) (EApp (EVar (Var 1)) (EVar (Var 2)))
	, ELambda (Var 3) (EApp (EVar (Var 4)) (EVar (Var 3)))
	]
 
			 