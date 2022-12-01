
Clean (
	:: *State :== Int ;
	:: IntPointer :== Int
	)

void *my_malloc (int size_in_bytes); /* allocates size_in_bytes bytes */
Clean (my_malloc :: Int State -> (IntPointer,State) )

int my_free (void*); /* frees memory allocated by my_malloc */
Clean (my_free :: IntPointer State -> (Int,State) )

int *store_int (int *p,int offset,int value); /* p [offset] = value */
Clean (store_int :: IntPointer Int Int State -> (IntPointer,State) )

int add_p (int *p1,int *p2); /* returns *p1 + *p2 */
Clean (add_p :: IntPointer IntPointer State -> (Int,State) )

