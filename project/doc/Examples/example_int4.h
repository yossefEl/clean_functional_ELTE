
Clean (
	:: *State :== Int ;
	:: IntPointer :== Int
	)

void *my_malloc (int size_in_bytes); /* allocates size_in_bytes bytes */
int my_free (void*); /* frees memory allocated by my_malloc */
void store_int (int *p,int offset,int value); /* p [offset] = value */
int add_p (int *p1,int *p2); /* returns *p1 + *p2 */

Clean (
	my_malloc :: Int State -> (IntPointer,State);
	my_free :: IntPointer State -> (Int,State) ;
	store_int :: IntPointer Int Int State -> State ;
	add_p :: IntPointer IntPointer State -> (Int,State)
	)
