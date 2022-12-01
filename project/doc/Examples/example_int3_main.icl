module example_int3_main;

import StdEnv;

import example_int3;

Start
	# state=0;
	# (p,state) = my_malloc (2<<2) state; // allocate memory for 2 integers;
	| p==0
		= abort "Out of memory";
	# (p,state) = store_int p 0 10 state; // p[0]=10
	# (p,state) = store_int p 1 20 state; // p[1]=20
	# (s,state) = add_p (p+0) (p+4) state; // add p[0] and p[1]
	# (r,state) = my_free p state; // release_memory
	| r<>0 // force evaluation of my_free and test for error
		= abort "Function my_free failed";
	= s;
