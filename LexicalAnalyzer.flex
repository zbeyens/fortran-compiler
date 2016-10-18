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

Number			= [0-9] //0
AlphaNumeric	= {Alpha}|{Number}
VarName 		= {Alpha}{AlphaNumeric}* //0
EndLine         = ({Space}* "\r"? "\n"+)+ {Space}* //0

/*ProgramBegin    = ^{Space}* "PROGRAM" {Space}+*/
//Program
ProgramBegin    = {Space}* "PROGRAM" {Space}+
ProgramEnd      = {Space}* "END"
//Vars - VarList
Vars            = "INTEGER" {Space}+
Comma           = {Space}*","{Space}*
//Code
EqualSign       = {Space}*"="{Space}*
/*Assign          = {VarName} {EqualSign}*/
Read            = "READ"{Space}*"*"

%state YYINITIAL, PROGNAME, PROGRAM_EOL, VARS, VARLIST, VARLIST_NEXT, CODE, INSTRUCTION, READ_COMMA

%%//Identification of tokens
<YYINITIAL> {
    {ProgramBegin} {
        System.out.println("token: " + yytext() + "\tlexical unit: " + "PROGRAM");
        return symbol(LexicalUnit.PROGRAM, yytext());
        yybegin(PROGNAME);
    }
}

<PROGNAME> {
    {VarName} {
        System.out.println("token: " + yytext() + "\tlexical unit: " + "VARNAME");
        yybegin(PROGRAM_EOL);
    }
}

<PROGRAM_EOL> {
    {EndLine} {
        System.out.println("token: " + "\t" + "\tlexical unit: " + "ENDLINE");
        yybegin(VARS);
    }
}

<VARS> {
    {Vars} {
        System.out.println("token: " + yytext() + "\tlexical unit: " + "INTEGER");
        yybegin(VARLIST);
    }

    //ε
}

<VARLIST> {
    {VarName} {
        System.out.println("token: " + yytext() + "\tlexical unit: " + "VARNAME");
        yybegin(VARLIST_NEXT);
    }
}

<VARLIST_NEXT> {
    {Comma} {
        System.out.println("token: " + yytext() + "\tlexical unit: " + "COMMA");
        yybegin(VARLIST);
    }

    {EndLine} {
        System.out.println("token: " + "\t" + "\tlexical unit: " + "ENDLINE");
        yybegin(INSTRUCTION);
    }
}

/*<CODE> {
    yybegin(INSTRUCTION);
}*/

<INSTRUCTION> {
    /*{Assign} {
        System.out.println("token: " + "\t" + "\tlexical unit: " + "ENDLINE");
        yybegin(INSTRUCTION);
    }*/

    {Read} {
        System.out.println("token: " + yytext() + "\tlexical unit: " + "READ");
        yybegin(READ_COMMA);
    }
}

<READ_COMMA> {
    {Comma} {
        System.out.println("token: " + yytext() + "\tlexical unit: " + "COMMA");
        yybegin(VARLIST);
    }

    {EndLine} {
        System.out.println("token: " + "\t" + "\tlexical unit: " + "ENDLINE");
        yybegin(CODE);
    }
}

\n {}
. {}

/*{VarName}       {symbol(LexicalUnit.VARNAME, yytext());}*/
