implementation module example_int2;

my_malloc :: !Int -> Int;
my_malloc a0 = code {
	ccall my_malloc "I:I"
}
// void* my_malloc (int size_in_bytes);

my_free :: !Int -> Int;
my_free a0 = code {
	ccall my_free "I:I"
}
// int my_free (void*);

store_int :: !Int !Int !Int -> Int;
store_int a0 a1 a2 = code {
	ccall store_int "III:I"
}
// int* store_int (int* p,int offset,int value);

add_p :: !Int !Int -> Int;
add_p a0 a1 = code {
	ccall add_p "II:I"
}
// int add_p (int* p1,int* p2);
