/*import LexicalUnit*/

%%// Options of the scanner
%class LexicalAnalyzer	//Name
%unicode		//Use unicode
%line			//Use line counter (yyline variable)
%column			//Use character counter by line (yycolumn variable)
%standalone

%{
    StringBuffer string = new StringBuffer();

    private void symbol(LexicalUnit type) {
        new Symbol(type, yyline, yycolumn);
        System.out.print(type);
    }
    private void symbol(LexicalUnit type, Object value) {
        new Symbol(type, yyline, yycolumn, value);
        System.out.print(type);
    }
%}

//Extended Regular Expressions
AlphaUpperCase	= [A-Z]
AlphaLowerCase	= [a-z]
Alpha			= {AlphaUpperCase}|{AlphaLowerCase}
Space           = "\t" | " "
Comma           = {Space}*","{Space}*

Number			= [0-9] //0
AlphaNumeric	= {Alpha}|{Number}
VarName 		= {Alpha}{AlphaNumeric}* //0
EndLine         = ({Space}* "\r"? "\n"+)+ {Space}* //0

/*ProgramBegin    = ^{Space}* "PROGRAM" {Space}+*/
ProgramBegin    = {Space}* "PROGRAM" {Space}+
ProgramEnd      = {Space}* "END"
Program         = {ProgramBegin} {VarName} {EndLine} {Vars} {Code} {ProgramEnd}

VarList         = {VarName}({Comma}{VarName})* //4,5
Vars           = "INTEGER" {Space}+

%state YYINITIAL, STATE1, STATE2, STATE3, STATE4, STATE5, STATE6

%%//Identification of tokens
<YYINITIAL> {
    {ProgramBegin} {
        System.out.println("token: " + yytext() + "\tlexical unit: " + "PROGRAM");
        yybegin(STATE1);
    }
}

<STATE1> {
    {VarName} {
        System.out.println("token: " + yytext() + "\tlexical unit: " + "VARNAME");
        yybegin(STATE2);
    }
}

<STATE2> {
    {EndLine} {
        System.out.println("token: " + "\t" + "\tlexical unit: " + "ENDLINE");
        yybegin(STATE3);
    }
}

<STATE3> {
    {Vars} {
        System.out.println("token: " + yytext() + "\tlexical unit: " + "INTEGER");
        yybegin(STATE4);
    }
}

<STATE4> {
    {VarName} {
        System.out.println("token: " + yytext() + "\tlexical unit: " + "VARNAME");
        yybegin(STATE5);
    }
}

<STATE5> {
    {Comma} {
        System.out.println("token: " + yytext() + "\tlexical unit: " + "COMMA");
        yybegin(STATE4);
    }

    {EndLine} {
        System.out.println("token: " + "\t" + "\tlexical unit: " + "ENDLINE");
        yybegin(STATE6);
    }
}


\n {}
. {}

/*{VarName}       {symbol(LexicalUnit.VARNAME, yytext());}*/
