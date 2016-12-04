import java.util.*;
import java.io.*;

%%// Options of the scanner
%class LexicalAnalyzer	//Name
%unicode		//Use unicode
%line			//Use line counter (yyline variable)
%column			//Use character counter by line (yycolumn variable)
%standalone

%{
    private ArrayList<Symbol> symbolTable = new ArrayList<Symbol>();
    public static ArrayList<Symbol> symList = new ArrayList<Symbol>();

    private Symbol symbol(LexicalUnit type, Object value) {
        Symbol sb = new Symbol(type, yyline, yycolumn, value);
        symList.add(sb);
        /*System.out.println(sb.toString());*/
        return sb;
    }

    //add an identifier in the symbolTable if not in the table
    private void addInTable(Symbol sb) {
        boolean inTable = false;
        for (Symbol symb : symbolTable) {
            if (sb.getValue().equals(symb.getValue())) {
                inTable = true;
            }
        }
        if (!inTable) {
            symbolTable.add(sb);
        }
    }

	public static Comparator<Symbol> SymbolComparator = new Comparator<Symbol>() {
	    public int compare(Symbol sb1, Symbol sb2) {
			String Name1 = (String) sb1.getValue();
			Name1=Name1.toUpperCase();
			String Name2 = (String) sb2.getValue();
			Name2=Name2.toUpperCase();
			//ascending order
			return Name1.compareTo(Name2);
	    }
	};


    //print all the identifiers with the line number where first occuring
    private void printSymbolTable() {
        System.out.println("Identifiers");
		Collections.sort(symbolTable,SymbolComparator);
        for (Symbol symb : symbolTable) {
            System.out.println(symb.getValue() + "\t" + symb.getLine());
        }
    }


%}

%eof{
    /*printSymbolTable();*/
%eof}

//Extended Regular Expressions

Alpha			= [a-zA-Z]
AlphaNumeric	= {Alpha}|[0-9]

VarName 		= {Alpha}{AlphaNumeric}*
Integer         = [Ii][Nn][Tt][Ee][Gg][Ee][Rr]
Number			= [0-9]+

Program         = [Pp][Rr][Oo][Gg][Rr][Aa][Mm]
End             = [Ee][Nn][Dd]

If              = [Ii][Ff]
Then            = [Tt][Hh][Ee][Nn]
EndIf           = {End}{If}
Else            = [Ee][Ll][Ss][Ee]

Not             = "."[Nn][Oo][Tt]"."
And             = "."[Aa][Nn][Dd]"."
Or              = "."[Oo][Rr]"."
Equal_Compare   = "."[Ee][Qq]"."
Greater_Equal   = "."[Gg][Ee]"."
Greater         = "."[Gg][Tt]"."
Smaller_Equal   = "."[Ll][Ee]"."
Smaller         = "."[Ll][Tt]"."
Different       = "."[Nn][Ee]"."

Do              = [Dd][Oo]
EndDo           = {End}{Do}

Print           = [Pp][Rr][Ii][Nn][Tt]"*"
Read            = [Rr][Ee][Aa][Dd]"*"

EndOfLine       = "\r"? "\n"
EndLine         = {EndOfLine}+ ([cC*dD!] .* {EndOfLine}+)* //infinite EOL + comments ignored

%state YYINITIAL, IDENTIFIERS
%%//Identification of tokens


<IDENTIFIERS> {
    {EndLine} {Symbol symb = symbol(LexicalUnit.ENDLINE, "\t");
    yybegin(YYINITIAL);}
}
{EndLine} {Symbol symb = symbol(LexicalUnit.ENDLINE, "\t");}

{Program} {Symbol symb = symbol(LexicalUnit.PROGRAM, yytext());}

{Then} {Symbol symb = symbol(LexicalUnit.THEN, yytext());}
{EndIf} {Symbol symb = symbol(LexicalUnit.ENDIF, yytext());}
//after ENDIF
{If} {Symbol symb = symbol(LexicalUnit.IF, yytext());}
{Else} {Symbol symb = symbol(LexicalUnit.ELSE, yytext());}
{Not} {Symbol symb = symbol(LexicalUnit.NOT, yytext());}
{And} {Symbol symb = symbol(LexicalUnit.AND, yytext());}
{Or} {Symbol symb = symbol(LexicalUnit.OR, yytext());}
//before =
{Equal_Compare} {Symbol symb = symbol(LexicalUnit.EQUAL_COMPARE, yytext());}
{Greater_Equal} {Symbol symb = symbol(LexicalUnit.GREATER_EQUAL, yytext());}
{Greater} {Symbol symb = symbol(LexicalUnit.GREATER, yytext());}
{Smaller_Equal} {Symbol symb = symbol(LexicalUnit.SMALLER_EQUAL, yytext());}
{Smaller} {Symbol symb = symbol(LexicalUnit.SMALLER, yytext());}
{Different} {Symbol symb = symbol(LexicalUnit.DIFFERENT, yytext());}

{EndDo} {Symbol symb = symbol(LexicalUnit.ENDDO, yytext());}
//after ENDDO
{Do} {Symbol symb = symbol(LexicalUnit.DO, yytext());}

{Print} {Symbol symb = symbol(LexicalUnit.PRINT, yytext());}
{Read} {Symbol symb = symbol(LexicalUnit.READ, yytext());}

"," {Symbol symb = symbol(LexicalUnit.COMMA, yytext());}
"=" {Symbol symb = symbol(LexicalUnit.EQUAL, yytext());}
"(" {Symbol symb = symbol(LexicalUnit.LEFT_PARENTHESIS, yytext());}
")" {Symbol symb = symbol(LexicalUnit.RIGHT_PARENTHESIS, yytext());}
"-" {Symbol symb = symbol(LexicalUnit.MINUS, yytext());}
"+" {Symbol symb = symbol(LexicalUnit.PLUS, yytext());}
//after READ, PRINT, ENDLINE
"*" {Symbol symb = symbol(LexicalUnit.TIMES, yytext());}
"/" {Symbol symb = symbol(LexicalUnit.DIVIDE, yytext());}

//after ENDDO, ENDIF
{End} {Symbol symb = symbol(LexicalUnit.END, yytext());}

<YYINITIAL> {
    {Integer} {Symbol symb = symbol(LexicalUnit.INTEGER, yytext());
                yybegin(IDENTIFIERS);}
    //progname, after all alpha symbols
    {VarName} {Symbol symb = symbol(LexicalUnit.VARNAME, yytext());}
}

<IDENTIFIERS> {
    //identifiers, after all alpha symbols
    {VarName} {Symbol symb = symbol(LexicalUnit.VARNAME, yytext());
        addInTable(symb);}
}

//after VARNAME
{Number} {Symbol symb = symbol(LexicalUnit.NUMBER, yytext());}

" " {}
. {System.out.println("ERROR");}
