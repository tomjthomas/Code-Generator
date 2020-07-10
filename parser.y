%{
	#include<stdio.h>
	#include<stdlib.h>
	#include<string.h>

	void yyerror(char *s);


	int label_count=0;                 /* Used for the creation of unique labels */
	char buffer[300];		   /* Temporary buffer to hold intermediate code  (written to file)*/


	void setValue(char s[],int n);    /* Enter symbol and corresponding value to  the symbol table */
	int getValue(char s[]);		   /* Get the value associated with  an identifier */
	void dis();			   /* Display the Symbol Table */
	int relOp(int a, int b, int c);	   /* Performs relational operation and returns result */
 
	char reg[7][10]={"t1","t2","t3","t4","t5","t6"};   /* Temporaries for holding values for IR Code */


	extern FILE *yyout;  		/* Pointer to the output file */
	extern char *yylex();


	/* The Symbol Table containing name and value */
	struct table
	{
		char name[10];
		int val;
	} symbol[53];

%}

%union{
	int no;
	char var[10];
	char code[100];
    }

%token <var> id  
%token <no> num
%type <code> condn assignment statement while_statement print_statement
%token print EXIT IF ELSE ptable WHILE
%type <no> start exp term 
%start start  
%left and or 
%left '>' '<' eq ne ge le '?' ':'
%left '+' '-' '%'
%left '*' '/'

%%

start	: EXIT ';'		{	exit(0);	}
	| print exp ';'		{ 
					printf("Printing: %d\n",$2);
					sprintf(buffer,"%s := %d;"
						       "\nprint %s;\n" ,
						reg[0],$2,reg[0]);
					fprintf(yyout,"%s\n" , buffer);
				}
	| start print exp ';'   { 
					printf("Printing: %d\n",$3); 
					sprintf(buffer,"%s := %d;"
						       "\nprint %s;\n" ,
						reg[0],$3,reg[0]);
					fprintf(yyout,"%s\n" , buffer); 
				}
	|  id '=' exp ';' 	{ 
					{setValue($1,$3);}
					
					sprintf(buffer,"%s := %d;"
							"\n %s := %s;\n" ,
							reg[0],$3,$1,reg[0]);
					fprintf(yyout,"%s\n" , buffer); 
				}

	| start  id '=' exp ';' { 
					{setValue($2,$4);} 
					sprintf(buffer,"%s := %d;"
							"\n %s := %s;\n" ,
							reg[0],$4,$2,reg[0]);
					fprintf(yyout,"%s\n" , buffer);
				}

	| condn			{ 
					fprintf(yyout,"%s\n" , $1); 
				}

	| start condn		{ 
					fprintf(yyout,"%s\n" , $2);
				}
	| while_statement	{ 
					fprintf(yyout,"%s\n" , $1);
				}

	| start while_statement { 	 
					fprintf(yyout,"%s\n" , $2);
				}
	| start EXIT ';'	{
					 exit(EXIT_SUCCESS);
				}
        		;

while_statement : WHILE '(' exp ')' '{' statement '}' 
				{ 
					sprintf(buffer,"label%d : IF NZ GOTO label%d"
							"\n %s\n JMP label%d"
							"\n label%d:\n" ,
							label_count,(label_count+1) ,$6,
							label_count,(label_count+1));
					strcpy($$,buffer);
					++label_count;
					++label_count;
				}

condn :  IF '(' exp ')' '{' statement '}'
                { 
                sprintf(buffer,"IF NZ GO TO label%d:"
                        "\n%slabel%d:" , label_count,$6 , label_count);
                    strcpy($$,buffer); 
                    ++label_count;
				}
	  |	 IF '(' exp ')'  '{' statement '}' ELSE '{' statement '}'
                { 
                    sprintf(buffer,"IF NZ GO TO label%d:"
                        "\n %s "
                        "\n JMP label%d "
                        "\n label%d:%s"
                        "\nlabel%d" ,
                        label_count,$6 , (label_count+1) ,
                        label_count,$10,(label_count+1));
                    strcpy($$,buffer);
                    ++label_count; 
                    ++label_count;
				}
				;

statement : assignment statement 
	  			{ 
					 strcat($1,$2);
					 strcpy($$,$1);
			    }
			| print_statement statement {  strcat($1,$2);  strcpy($$,$1); }
			| assignment		{ { strcpy($$,$1); } }
			| print_statement { {strcpy($$,$1);} }
			| condn statement {  strcat($1,$2); strcpy($$,$1); }
			| condn		{ { strcpy($$,$1); } }
			|';' { strcpy($$,"");	}    
			;  

print_statement : print exp ';' {  sprintf(buffer,"%s := %d;\nprint %s;\n",reg[0],$2,reg[0]); strcpy($$,buffer);  }

assignment : id '=' exp ';' { {setValue($1,$3);} sprintf(buffer,"%s := %d;\n%s := %s;\n",reg[0],$3,$1,reg[0]); strcpy($$,buffer); }

exp    	: term                  { {$$ = $1;}                    /*fprintf(yyout,"%s := %d;\n ",reg[0],$1);*/ ; } 
        | exp '+' exp           { {$$ = $1 + $3;}               /*fprintf(yyout,"%s := %d + %d;\n ",reg[0],$1,$3);*/ ; } 
        | exp '-' exp           { {$$ = $1 - $3;}               /*fprintf(yyout,"%s := %d - %d;\n ",reg[0],$1,$3);*/ ; }
        | exp '*' exp	        { {$$ = $1 * $3;}               /*fprintf(yyout,"%s := %d * %d;\n ",reg[0],$1,$3);*/ ; }
        | exp '/' exp	        { {$$ = $1 / $3;}               /*fprintf(yyout,"%s := %d / %d;\n ",reg[0],$1,$3);*/ ; }
        | exp '%'exp			{ {$$= $1 % $3;}}	
        | exp '>' exp			{ {$$ =relOp($1,$3,1);}        /*fprintf(yyout,"%s := %c > %d;\n ",reg[0],$1,$3); */; } 
        | exp '<' exp			{ {$$ =relOp($1,$3,2);}        /*fprintf(yyout,"%s := %c < %d;\n ",reg[0],$1,$3); */; }
        | exp eq exp			{ {$$ =relOp($1,$3,3);}        /*fprintf(yyout,"%s := %c eq %d;\n ",reg[0],$1,$3); */;}
        | exp ne exp			{ {$$ =relOp($1,$3,4);}	       /*fprintf(yyout,"%s := %c neq %d;\n ",reg[0],$1,$3); */;}
        | exp ge exp			{ {$$ =relOp($1,$3,5);}	       /*fprintf(yyout,"%s := %c ge %d;\n ",reg[0],$1,$3); */;}
        | exp le exp			{ {$$ =relOp($1,$3,6);}        /*fprintf(yyout,"%s := %c le %d;\n ",reg[0],$1,$3); */;}
        | '(' exp ')'			{ {$$ = $2;}                   /*fprintf(yyout,"%s := %d;\n ",reg[0],$2); */;}
        | exp and exp			{ {$$ =relOp($1,$3,7);}        /*fprintf(yyout,"%s := %c and %d;\n ",reg[0],$1,$3);*/ ;}
        | exp or exp			{ {$$ =relOp($1,$3,8);}        /*fprintf(yyout,"%s := %c or %d;\n ",reg[0],$1,$3);*/ ;}
        ;

term   	: num                {$$ = $1;}
	|id			{$$=getValue($1);}
;

%%

//RELATIONAL OPERATIONS
int relOp(int a , int b ,int op)
{
	switch(op)
	{
		case 1:if(a>b){return 1;} else{return 0;} break;
		case 2:if(a<b){return 1;} else{return 0;} break;
		case 3:if(a==b){return 1;} else{return 0;} break;
		case 4:if(a!=b){return 1;} else{return 0;} break;
		case 5:if(a>=b){return 1;} else{return 0;} break;
		case 6:if(a<=b){return 1;} else{return 0;} break;
		case 7:if(a>0 && b>0 ){return 1;}else{return 0;}break;
		case 8:if(a>0 || b>0 ){return 1;}else{return 0;}break;
	}
}


//DISPLAYS THE SYMBOL TABLE
void dis()
{
	int i;
	printf("index\tvar\tval\n");
	for(i=0;i<53;i++)
	{
 		if(symbol[i].val!=-101)
 			printf("%d\t%s\t%d\n",i,symbol[i].name,symbol[i].val);
	}
}

//INSERTS VALUE INTO THE SYMBOL TABLE
void setValue(char str[],int n)
{
	int index,i;
	index=str[0]%53;
	i=index;
	if(strcmp(str,symbol[i].name)==0||symbol[i].val==-101)
	{
		symbol[index].val=n;
		strcpy(symbol[index].name,str);
	}
	else
	{
		i=(i+1)%53;
 		while(i!=index)
		{
			if(strcmp(str,symbol[i].name)==0||symbol[i].val==-101)
			{
				symbol[i].val=n;
				strcpy(symbol[i].name,str);
				break;
			}
			i=(i+1)%53;
		}
	}

}


//VALUES FROM SYMBOL TABLE
int getValue(char str[])
{
	int index,i;
	index=str[0]%53;
	i=index;
	if(strcmp(str,symbol[index].name)==0)
	{
		return(symbol[index].val);
	}
	else
	{
		i=(i+1)%53;
 		while(i!=index)
		{
			if(strcmp(str,symbol[i].name)==0)
			{
				return (symbol[i].val);
				break;
			}
			i=(i+1)%53;
		}
		if(i==index)
		{
			printf("not initialised.");
		}
	}

}

void yyerror (char *s) 
{
	fprintf (stdout, "%s\n", s);
} 


int main()
{

	// Initialise the Symbol Table
 	for(i=0;i<53;i++)
	{
		symbol[i].val=-101;
	}

	yyout = fopen("output.txt","a");
	
	/* if(yyout==NULL)
	{
		printf("error!!");
	}
	else
	{
		printf("file opened");
	} */


	//fprintf(yyout,"%s",reg[0]);
	//fprintf("\n%s",ftell(yyout));


 	return yyparse();

}
