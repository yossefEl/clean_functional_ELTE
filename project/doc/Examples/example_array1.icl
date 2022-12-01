implementation module example_array1;

sum_int_array :: !{#Int} -> Int;
sum_int_array a0 = code {
	ccall sum_int_array "A:I"
}
// int sum_int_array (CleanIntArray a);

sum_real_array :: !{#Real} -> Real;
sum_real_array a0 = code {
	ccall sum_real_array "A:R"
}
// double sum_real_array (CleanRealArray a);

first_different_char_index :: !{#Char} !{#Char} -> Int;
first_different_char_index a0 a1 = code {
	ccall first_different_char_index "ss:I"
}
// int first_different_char_index (CleanCharArray a1,CleanCharArray a2);
