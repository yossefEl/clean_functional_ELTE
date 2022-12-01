
#include <windows.h>

#include "example_int2.h"
/* or:
#	include "Clean.h"
#	include "example_int3.h"
*/

void *my_malloc (int size_in_bytes)
{
	return GlobalAlloc (0,size_in_bytes);
}

int my_free (void *p)
{
	return GlobalFree(p)!=NULL;
}

int *store_int (int *p,int offset,int value)
{
	p[offset]=value;
	return p;
}

int add_p (int *p1,int *p2)
{
	return *p1 + *p2;
}

