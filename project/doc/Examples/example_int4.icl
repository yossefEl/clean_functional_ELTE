implementation module example_int4;

:: *State :== Int;
:: IntPointer :== Int;

my_malloc :: !Int !State -> (!IntPointer,!State);
my_malloc a0 a1 = code {
	ccall my_malloc "I:I:I"
}
// void* my_malloc (int size_in_bytes);

my_free :: !IntPointer !State -> (!Int,!State);
my_free a0 a1 = code {
	ccall my_free "I:I:I"
}
// int my_free (void*);

store_int :: !IntPointer !Int !Int !State -> State;
store_int a0 a1 a2 a3 = code {
	ccall store_int "III:V:I"
}
// void store_int (int* p,int offset,int value);

add_p :: !IntPointer !IntPointer !State -> (!Int,!State);
add_p a0 a1 a2 = code {
	ccall add_p "II:I:I"
}
// int add_p (int* p1,int* p2);
