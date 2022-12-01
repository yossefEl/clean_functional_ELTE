module example_string1_main;

import StdEnv;
import example_string1;

/*
	string_to_uppercase_with_side_effect updates the string. This is only allowed if this 
	is the only reference to the string (i.e. the string is unique).
	We cannot make the string unique with the htoclean tool (unless we copy the string). Therefore,
	we define string_to_uppercase to make sure that the string is unique and use this function instead.
	We can return the string as a unique string by cheating using observation typing.
*/

string_to_uppercase :: !*String -> (!Int,!*String);
string_to_uppercase string
	#! n=string_to_uppercase_with_side_effect string; // evaluate and pretend we observe the string
	= (n,string);

Start = (spaces_string 18+++"vwxyz\n"+++
		 spaces_string 20+++"xyz\n",
		 hello_string_from_c,"\n",
		string_to_uppercase {'A','a','B','b','C','c'});
// we use {'A','a','B','b','C','c'} because "AaBbCc" is not unique
