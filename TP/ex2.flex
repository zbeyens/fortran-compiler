%%
%class ex2
%unicode
%line
%standalone

%{
    private int alphaNumCharCounter = 0;
    private int alphaNumWordCounter = 0;
    private int alphaNumLineCounter = 0;
    private boolean alphaNumCharFound = false;
    private boolean nonAlphaNumCharFound = false;
    private boolean isAWord = true;
    private boolean isEmptyWord = true;

    private void alphaNumChar() {
        alphaNumCharCounter++;
        isEmptyWord = false;
        alphaNumCharFound = true;
    }

    private void spaces() {
        if (isAWord && !isEmptyWord) {
            alphaNumWordCounter++;
        }
        isAWord = true;
        isEmptyWord = true;
    }

    private void endOfLine() {
        if (isAWord && !isEmptyWord) {
            alphaNumWordCounter++;
        }
        isAWord = true;
        isEmptyWord = true;

        if (nonAlphaNumCharFound) {
            nonAlphaNumCharFound = false;
        } else if (alphaNumCharFound) {
            alphaNumLineCounter++;
        }
        alphaNumCharFound = false;
    }

    private void nonAlphaNumChar() {
        isAWord = false;
        isEmptyWord = false;
        nonAlphaNumCharFound = true;
    }
%}

%eof{
    System.out.println("Char : " + alphaNumCharCounter);
    System.out.println("Word : " + alphaNumWordCounter);
    System.out.println("Line : " + alphaNumLineCounter);
%eof}

//"" or not
AlphaNum = [a-zA-Z0-9]
EndOfLine = \r?\n
Space = \t|" "

%%
{AlphaNum} {alphaNumChar();}
{Space}+ {spaces();}
<<EOF>> {endOfLine(); return 0;}
{EndOfLine} {endOfLine();}
. {nonAlphaNumChar();}
