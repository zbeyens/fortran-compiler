%%
%class ex1
%unicode
%line
%standalone

EndOfLine = "\r"?"\n"
Line = .*{EndOfLine}
%%
{Line} {System.out.println(yyline+yytext());}
