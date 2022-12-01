definition module example_int2;

my_malloc :: !Int -> Int;
// void* my_malloc (int size_in_bytes);
my_free :: !Int -> Int;
// int my_free (void*);
store_int :: !Int !Int !Int -> Int;
// int* store_int (int* p,int offset,int value);
add_p :: !Int !Int -> Int;
// int add_p (int* p1,int* p2);
