definition module example_array1;

sum_int_array :: !{#Int} -> Int;
// int sum_int_array (CleanIntArray a);
sum_real_array :: !{#Real} -> Real;
// double sum_real_array (CleanRealArray a);
first_different_char_index :: !{#Char} !{#Char} -> Int;
// int first_different_char_index (CleanCharArray a1,CleanCharArray a2);
