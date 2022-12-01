module toStr

import StdGeneric, StdEnv, GenMap

generic gToString a :: String a -> String
gToString{|Int|} sep x 					= toString x
gToString{|Char|} sep x 				= toString x
gToString{|Bool|} sep x 				= toString x
gToString{|Real|} sep x 				= toString x
gToString{|UNIT|} sep UNIT 				= ""
gToString{|PAIR|} fx fy sep (PAIR x y) 	= fx sep x +++ sep +++ fy sep y
gToString{|EITHER|} fl fr sep (LEFT x) 	= fl sep x
gToString{|EITHER|} fl fr sep (RIGHT x) = fr sep x
gToString{|(->)|} fa fr sep f 			= "<fun>"
gToString{|CONS of d|} fx sep (CONS x) 
	| d.gcd_arity == 0
		= d.gcd_name 
		= "(" +++ d.gcd_name +++ " " +++ fx " " x +++ ")"
gToString{|RECORD of d|} fx sep (RECORD x) 
	= "{" +++ d.grd_name +++ " | " +++ fx ", " x +++ "}"
gToString{|FIELD of {gfd_name}|} fx sep (FIELD x)	 
	= gfd_name +++ "=" +++ fx sep x
gToString{|OBJECT|} fx sep (OBJECT x)	 
	= fx sep x
	
gToString {|{}|} fx sep xs 				= "{" +++ listToStr fx [x\\x<-:xs] +++ "}"
gToString {|{!}|} fx sep xs 			= "{!" +++ listToStr fx [x\\x<-:xs] +++ "}"
//gToString {|{#}|} fx sep xs 			= "{#" +++ listToStr fx [x\\x<-:xs] +++ "}"
gToString{|String|} sep xs 				= "\"" +++ xs +++ "\"" 
gToString{|[]|} f sep xs 				= "[" +++ listToStr f xs +++ "]"
gToString{|(,)|} f1 f2 sep (x1, x2) 
	= "("+++f1 sep x1+++", "+++f2 sep x2+++")"
gToString{|(,,)|} f1 f2 f3 sep (x1, x2, x3) 
	= "("+++f1 sep x1+++", "+++f2 sep x2+++", "+++f3 sep x3+++")"
gToString{|(,,,)|} f1 f2 f3 f4 sep (x1, x2, x3, x4) 
	= "("+++f1 sep x1+++", "+++f2 sep x2+++", "+++f3 sep x3+++", "+++f4 sep x4+++")"

listToStr :: (.String -> .(.a -> String)) ![.a] -> String
listToStr f [] = ""
listToStr f [x] = f "" x
listToStr f [x:xs] = f "" x +++ ", " +++ listToStr f xs	

toStr = gToString{|*|} ""

:: Tree a b = Tip a | Bin b (Tree a b) (Tree a b)
:: Rose a = Rose a .[Rose a]
:: Record = {rec_x :: Int, rec_y :: Int}

derive gToString Tree, Rose, Record

Start = "\n" +++ foldr (\x y -> x +++ "\n" +++ y) ""
 	[ toStr [1,2,3]
	, toStr (Rose 1 [Rose 2 [], Rose 3 []])
	, toStr {rec_x = 1, rec_y = 2}
	]
