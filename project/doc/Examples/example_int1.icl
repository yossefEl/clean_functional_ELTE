implementation module example_int1;

add :: !Int !Int -> Int;
add a0 a1 = code {
	ccall add "II:I"
}
// int add (int v1,int v2);
