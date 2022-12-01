module example_int2_main;

import StdEnv;

import example_int2;

Start
	# p=my_malloc (2<<2); // allocate memory for 2 integers;
	| p==0
		= abort "Out of memory";
	# p=store_int p 0 10; // p[0]=10
	# p=store_int p 1 20; // p[1]=20
	# s=add_p (p+0) (p+4); // add p[0] and p[1]
	| s==s // force evaluation of add, before deallocating memory !
	# r=my_free p; // release_memory
	| r<>0 // force evaluation of my_free and test for error
		= abort "Function my_free failed";
	= s;
