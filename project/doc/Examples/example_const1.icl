implementation module example_const1;

compute :: !Int !Int !Int -> Int;
compute a0 a1 a2 = code {
	ccall compute "III:I"
}
// int compute (int operator,int arg1,int arg2);
ADD:==10;
SUB:==20;
AND:==30;
OR:==40;
ADD_OP:=='+';
SUB_OP:=='-';
AND_OP:=='&';
OR_OP:=='|';
