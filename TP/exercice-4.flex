
%%// Options of the scanner

%class Lexer4	//Name
%unicode			//Use unicode
%column			//Use character counter (by line)
%standalone		//Tell that Jflex don't use a parser

%{
	private String replacement = "compiler";
%}

//Extended Regular Expressions

Space				= "\t" | " "
EndOfLine		= "\r"?"\n"	
Line				= .*{EndOfLine}

//Declare exclusive states
%xstate YYINITIAL, A_STATE, B_STATE, C_STATE

%% //Identification of tokens

//switch between mode, default : YYINITIAL

<YYINITIAL> {
	^"a"			{replacement = "ewww"; System.out.print(yytext());}
	^"b"			{replacement = "???"; System.out.print(yytext());}
	^"c"			{replacement = "profit!!!"; System.out.print(yytext());}
	{EndOfLine}	{ replacement = "compiler"; System.out.print(yytext()); }
	
	{Space}"compiler" / ({Space}|{EndOfLine}) { System.out.print(yytext().replaceAll("compiler",replacement)); }
	
	.				{System.out.print(yytext());}
}

