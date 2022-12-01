implementation module example_enum1;

compute :: !Int !Int !Int -> Int;
compute a0 a1 a2 = code {
	ccall compute "III:I"
}
// int compute (int operator,int arg1,int arg2);
ADD:==0;
SUB:==1;
AND:==2;
OR:==3;
