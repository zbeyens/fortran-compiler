%%
%class ex4
%unicode
%line
%standalone

%xstate YYINITIAL, AS, BS, CS
/*Comments = "{"[^"}"]*"}"*/
Word = compiler
EOL = "\r"?"\n"

%%
/*{Comments} {System.out.print(yytext());}*/
<YYINITIAL> {
    ^a {yybegin(AS); System.out.print(yytext());}
    ^b {yybegin(BS); System.out.print(yytext());}
    ^c {yybegin(CS); System.out.print(yytext());}
}

<AS> {
    {Word} {System.out.print("ewww");
            yybegin(AS);}
    {EOL} {System.out.println(); yybegin(YYINITIAL);}
}

<BS> {
    {Word} {System.out.print("???");
            yybegin(BS);}
    {EOL} {System.out.println(); yybegin(YYINITIAL);}
}

<CS> {
    {Word} {System.out.print("profit!!!");
            yybegin(CS);}
    {EOL} {System.out.println(); yybegin(YYINITIAL);}
}
