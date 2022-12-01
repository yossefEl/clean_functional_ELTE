definition module _SystemArray

class Array .a e where
	select				:: !.(a .e) !Int	-> .e
	uselect				:: !u:(a e) !Int	-> *(e, !u:(a e))
	size				:: !.(a .e)			-> Int
	usize				:: !u:(a .e)		-> *(!Int, !u:(a .e))
	update				:: !*(a .e) !Int .e -> *(a .e)	
	createArray			:: !Int e			-> *(a e)
	_createArray		:: !Int				-> *(a .e)
	replace				:: !*(a .e) !Int .e -> *(.e, !*(a .e))

instance Array {!} a where
	uselect :: !u:{! e} !Int -> *(!e, !u:{! e})
	update :: !*{! .e} !Int !.e -> *{! .e}
	createArray :: !Int !e -> *{! e}
	replace :: !*{! .e} !Int !.e -> *(!.e, !*{! .e})

instance Array {#} Int where
	uselect :: !u:{# Int} !Int -> *(!Int, !u:{# Int})
	update :: !*{# e:Int} !Int !e:Int -> *{# e:Int}
	createArray :: !Int !Int -> *{# Int}
	replace :: !*{# e:Int} !Int !e:Int -> *(!e:Int, !*{# e:Int})

instance Array {#} Char where
	uselect :: !u:{# Char} !Int -> *(!Char, !u:{# Char})
	update :: !*{# e:Char} !Int !e:Char -> *{# e:Char}
	createArray :: !Int !Char -> *{# Char}
	replace :: !*{# e:Char} !Int !e:Char -> *(!e:Char, !*{# e:Char})

instance Array {#} Real where
	uselect :: !u:{# Real} !Int -> *(!Real, !u:{# Real})
	update :: !*{# e:Real} !Int !e:Real -> *{# e:Real}
	createArray :: !Int !Real -> *{# Real}
	replace :: !*{# e:Real} !Int !e:Real -> *(!e:Real, !*{# e:Real})

instance Array {#} Bool where
	uselect :: !u:{# Bool} !Int -> *(!Bool, !u:{# Bool})
	update :: !*{# e:Bool} !Int !e:Bool -> *{# e:Bool}
	createArray :: !Int !Bool -> *{# Bool}
	replace :: !*{# e:Bool} !Int !e:Bool -> *(!e:Bool, !*{# e:Bool})

instance Array {#} {#.a} where
	uselect :: !u:{#{#.a}} !Int -> *(!{#.a},!u:{#{#.a}})
	update :: !*{#u:{#.a}} !Int !u:{#.a} -> *{#u:{#.a}}
	createArray :: !Int !{#.a} -> *{# {#.a}}
	replace :: !*{#u:{#.a}} !Int !u:{#.a} -> *(!u:{#.a},!*{#u:{#.a}})

instance Array {#} {!.a} where
	uselect :: !u:{#{!.a}} !Int -> *(!{!.a},!u:{#{!.a}})
	update :: !*{#u:{!.a}} !Int !u:{!.a} -> *{#u:{!.a}}
	createArray :: !Int !{!.a} -> *{# {!.a}}
	replace :: !*{#u:{!.a}} !Int !u:{!.a} -> *(!u:{!.a},!*{#u:{!.a}})

instance Array {#} {.a} where
	uselect :: !u:{#{.a}} !Int -> *(!{.a},!u:{#{.a}})
	update :: !*{#u:{.a}} !Int !u:{.a} -> *{#u:{.a}}
	createArray :: !Int !{.a} -> *{# {.a}}
	replace :: !*{#u:{.a}} !Int !u:{.a} -> *(!u:{.a},!*{#u:{.a}})

instance Array {#} {32#.a} where
	uselect :: !u:{#{32#.a}} !Int -> *(!{32#.a},!u:{#{32#.a}})
	update :: !*{#u:{32#.a}} !Int !u:{32#.a} -> *{#u:{32#.a}}
	createArray :: !Int !{32#.a} -> *{# {32#.a}}
	replace :: !*{#u:{32#.a}} !Int !u:{32#.a} -> *(!u:{32#.a},!*{#u:{32#.a}})

instance Array {#} a where
	uselect :: !u:{# e} !Int -> *(!e, !u:{# e})
	update :: !*{# .e} !Int !.e -> *{# .e}
	createArray :: !Int !e -> *{# e}
	replace :: !*{# .e} !Int !.e -> *(!.e, !*{# .e})

instance Array {32#} Int where
	uselect :: !u:{32# Int} !Int -> *(!Int, !u:{32# Int})
	update :: !*{32# e:Int} !Int !e:Int -> *{32# e:Int}
	createArray :: !Int !Int -> *{32# Int}
	replace :: !*{32# e:Int} !Int !e:Int -> *(!e:Int, !*{32# e:Int})

instance Array {32#} Real where
	uselect :: !u:{32# Real} !Int -> *(!Real, !u:{32# Real})
	update :: !*{32# e:Real} !Int !e:Real -> *{32# e:Real}
	createArray :: !Int !Real -> *{32# Real}
	replace :: !*{32# e:Real} !Int !e:Real -> *(!e:Real, !*{32# e:Real})

instance Array {32#} a where
	uselect :: !u:{32# e} !Int -> *(!e, !u:{32# e})
	update :: !*{32# .e} !Int !.e -> *{32# .e}
	createArray :: !Int !e -> *{32# e}
	replace :: !*{32# .e} !Int !.e -> *(!.e, !*{32# .e})

instance Array {} a
