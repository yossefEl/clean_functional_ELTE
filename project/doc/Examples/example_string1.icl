implementation module example_string1;

spaces_string :: !Int -> String;
spaces_string a0 = code {
	ccall spaces_string "I:VS"
}
// void spaces_string (int n_spaces,CleanString*);

hello_string_from_c :: String;
hello_string_from_c  = code {
	ccall hello_string_from_c ":VS"
}
// void hello_string_from_c (CleanString*);

string_to_uppercase_with_side_effect :: !String -> Int;
string_to_uppercase_with_side_effect a0 = code {
	ccall string_to_uppercase_with_side_effect "S:I"
}
// int string_to_uppercase_with_side_effect (CleanString);
