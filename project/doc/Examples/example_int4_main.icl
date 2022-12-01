module example_int4_main;

import StdEnv;

import example_int4;

(>:) infixl;
(>:) f g:==g f;

Start
	# state=0;
	# (p,state) = my_malloc (2<<2) state; // allocate memory for 2 integers;
	| p==0
		= abort "Out of memory";
	# (s,state) = state
					>: store_int p 0 10 // p[0]=10
					>: store_int p 1 20  // p[1]=20
					>: add_p (p+0) (p+4); // add p[0] and p[1]
	# (r,state) = my_free p state; // release_memory
	| r<>0 // force evaluation of my_free and test for error
		= abort "Function my_free failed";
	= s;
