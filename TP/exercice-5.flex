import java_cup.runtime.*;//Loading cup (yacc) library

%%// Options of the scanner

%class Lexer5	//Name
%unicode			//Use unicode
%line				//Use line counter (yyline variable)
%column			//Use character counter by line (yycolumn variable)
%cup				//Use with CUP instead of the standalone mode

//Extended Regular Expressions


AlphaUpperCase	= [A-Z]
AlphaLowerCase	= [a-z]
Alpha				= {AlphaUpperCase}|{AlphaLowerCase}
Numeric			= [0-9]
AlphaNumeric		= {Alpha}|{Numeric}

Sign				= [+-]
Integer			= {Sign}?([1-9][0-9]*)|0
Decimal			= \.[0-9]*
Exponent			= [eE]{Integer}
Real				= {Integer}{Decimal}?{Exponent}?
Identifier		= {Alpha}{AlphaNumeric}*

%%//Identification of tokens

//Relational operators
"!"				{return new Symbol(sym.NOT,yyline, yycolumn);}
"=="				{return new Symbol(sym.EQUALS,yyline, yycolumn);}
"!="				{return new Symbol(sym.NOTEQUALS,yyline, yycolumn);}
">"				{return new Symbol(sym.GREATER,yyline, yycolumn);}
">="				{return new Symbol(sym.EGREATER,yyline, yycolumn);}
"<"				{return new Symbol(sym.LOWER,yyline, yycolumn);}
"<="				{return new Symbol(sym.ELOWER,yyline, yycolumn);}

// If/Else keywords
"if"				{return new Symbol(sym.IF,yyline, yycolumn);}
"then"			{return new Symbol(sym.THEN,yyline, yycolumn);}
"else"			{return new Symbol(sym.ELSE,yyline, yycolumn);}


//Decimal number in scientific notation
{Real}			{return new Symbol(sym.FLOAT,yyline, yycolumn, new Double(yytext()));}

//C99 variable identifier
{Identifier}	{return new Symbol(sym.C99VAR,yyline, yycolumn, yytext());}
