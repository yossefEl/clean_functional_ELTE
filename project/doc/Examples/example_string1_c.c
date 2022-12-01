 
#include "Clean.h"
#include "example_string1.h"

#define MAX_SPACES_STRING_SIZE 1024

static CleanStringVariable(result_string,MAX_SPACES_STRING_SIZE);
 
void spaces_string (int n_spaces,CleanString *result_string_p)
{
	char *result_string_characters;
	int i;
	
	if (n_spaces>MAX_SPACES_STRING_SIZE)
		n_spaces=MAX_SPACES_STRING_SIZE;
	
	CleanStringLength(result_string) = n_spaces;

	result_string_characters = CleanStringCharacters(result_string);
	
	for (i=0; i<n_spaces; ++i)
		result_string_characters[i]=' ';
	
	*result_string_p = (CleanString)result_string;
}

static struct {int length; char chars[5+1]; } hello_string = {5,"hello"};

void hello_string_from_c (CleanString *hello_string_p)
{
	*hello_string_p=(CleanString)&hello_string;
}

int string_to_uppercase_with_side_effect (CleanString string)
{
	int i,length,n_lowercase_characters;
	char *characters;
	
	length=CleanStringLength(string);
	characters=CleanStringCharacters(string);
	n_lowercase_characters=0;
	
	for (i=0; i<length; ++i){
		char c;
		
		c=characters[i];
		if ((unsigned)(c-'a')<26u){
			characters[i]=c+('A'-'a');
			++n_lowercase_characters;
		}
	}
	
	return n_lowercase_characters;
}
