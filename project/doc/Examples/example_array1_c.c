
#include "Clean.h"

int sum_int_array (CleanIntArray a)
{
	int sum,s,i;
	
	s=CleanIntArraySize (a);
	
	sum=0;
	for (i=0; i<s; ++i)
		sum+=a[i];
	
	return sum;
}

double sum_real_array (CleanRealArray a)
{
	double sum;
	int s,i;
	
	s=CleanRealArraySize (a);
	
	sum=0;
	for (i=0; i<s; ++i)
		sum+=a[i];
	
	return sum;
}

int first_different_char_index (CleanCharArray a1,CleanCharArray a2)
{
	int s1,s2,i;
	
	s1=CleanCharArraySize (a1);
	s2=CleanCharArraySize (a2);
	
	for (i=0; i<s1 && i<s2; ++i)
		if (a1[i]!=a2[i])
			return i;
	
	if (s1!=s2)
		return i;
	else
		return -1;
}

int _fltused;
