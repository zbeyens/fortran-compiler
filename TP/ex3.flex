%%
%class ex3
%unicode
%line
%standalone

%xstate YYINITIAL, COMMENTS
/*Comments = "{"[^"}"]*"}"*/

%%
/*{Comments} {System.out.print(yytext());}*/
<YYINITIAL> {
    ^"{" {yybegin(COMMENTS);}
}
<COMMENTS> {
    "}" {yybegin(YYINITIAL);}
    [^o"}"]+ {System.out.print(yytext());}
}
. {}
\n {}
