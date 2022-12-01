definition module example_int3;

:: *State :== Int;
:: IntPointer :== Int;
my_malloc :: !Int !State -> (!IntPointer,!State);
// void* my_malloc (int size_in_bytes);
my_free :: !IntPointer !State -> (!Int,!State);
// int my_free (void*);
store_int :: !IntPointer !Int !Int !State -> (!IntPointer,!State);
// int* store_int (int* p,int offset,int value);
add_p :: !IntPointer !IntPointer !State -> (!Int,!State);
// int add_p (int* p1,int* p2);
