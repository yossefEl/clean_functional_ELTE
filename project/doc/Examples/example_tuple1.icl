implementation module example_tuple1;

import example_type1;

make_complex_int :: !Int !Int -> ComplexInt;
make_complex_int a0 a1 = code {
	ccall make_complex_int "II:VII"
}
// void make_complex_int (int re,int im,int* re_p,int* im_p);

add_complex_int :: !ComplexInt !ComplexInt -> ComplexInt;
add_complex_int a0 a1 = code {
	ccall add_complex_int "IIII:VII"
}
// void add_complex_int (int re1,int im1,int re2,int im2,int* re_p,int* im_p);
