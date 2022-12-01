
#include "Clean.h"
#include "example_const1.h"

int compute (int operator,int arg1,int arg2)
{
	switch (operator){
		case ADD: return arg1+arg2;
		case SUB: return arg1-arg2;
		case AND: return arg1&arg2;
		case OR:  return arg1|arg2;
	}
	return 0;
}
