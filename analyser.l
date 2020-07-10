%{
	#include "parser.tab.h"
	#include<string.h>
	#include <stdio.h>
	#include<stdlib.h>
	
%}
	
%%

"&"				        { 	return and; 	}
"|"				        { 	return or; 	}
"if"				    {	return IF;	}
"else"				    {	return ELSE;	}
"=="			  	    {	return eq;	}
"!="			  	    {	return ne;	}
"<="			        {	return le;	}
">="			  	    {	return ge;	}
"while"                 {   return WHILE;   }
"exit"			    	{	return EXIT;	} 		
"print"			 	    {	return print;	} 
"ptable"			    { 	return ptable;	}
[0-9]+				    {	
                            yylval.no=atoi(yytext) ; 
					        return num;
	                    }
[a-z]([a-z]|[0-9])*		{	
					        strcpy(yylval.var,yytext); 
					        return id;
				        } 
[ \t\n]					;
[- + ; ( ) * / = < > { } % ? : ]    {  return *yytext;  }
.				        {   printf("invalid char.");  }

%%

int yywrap()
{
return 1;
}


