
#include "Clean.h"
#include "example_tuple1.h"

void make_complex_int (int re,int im,int *re_p,int *im_p)
{
	*re_p=re;
	*im_p=im;
}

void add_complex_int (int re1,int im1,int re2,int im2,int *re_p,int *im_p)
{
	*re_p=re1+re2;
	*im_p=im1+im2;
}
