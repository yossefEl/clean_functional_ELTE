
#include <windows.h>

#include "Clean.h"
#include "example_int4.h"

void *my_malloc (int size_in_bytes)
{
	return GlobalAlloc (0,size_in_bytes);
}

int my_free (void *p)
{
	return GlobalFree(p)!=NULL;
}

void store_int (int *p,int offset,int value)
{
	p[offset]=value;
}

int add_p (int *p1,int *p2)
{
	return *p1 + *p2;
}

