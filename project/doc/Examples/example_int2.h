
void *my_malloc (int size_in_bytes); /* allocates size_in_bytes bytes */
int my_free (void*); /* frees memory allocated by my_malloc */

int *store_int (int *p,int offset,int value); /* p [offset] = value */

int add_p (int *p1,int *p2); /* returns *p1 + *p2 */
