module example_const1_main;

import example_const1;

char_to_op ADD_OP = ADD;
char_to_op SUB_OP = SUB;
char_to_op AND_OP = AND;
char_to_op OR_OP  = OR;

Compute op a1 a2 = compute (char_to_op op) a1 a2;

Start = (Compute '+' 1 2,Compute '-' 1 2,Compute '&' 1 2,Compute '|' 1 2);
