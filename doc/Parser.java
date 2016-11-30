import java.util.*;
import java.io.*;

public class Parser {
private ArrayList<Symbol> symList;
private LexicalUnit tok;
private int curTok = 0;

public Parser() {
    this.symList = LexicalAnalyzer.symList;
}

public void parse() {
    nextsym();
    program();
}

void nextsym() {
    tok = symList.get(curTok).getType();
    curTok++;
}

boolean match(LexicalUnit t) {
    if (tok == t) {
        nextsym();
        return true;
    } else {
        return false;
    }
}

boolean program() {
    int r = 1;

    switch (tok) {
        case PROGRAM:
            printRule(r);

            if (match(LexicalUnit.PROGRAM))
                if (match(LexicalUnit.VARNAME))
                    if (match(LexicalUnit.ENDLINE))
                        if (Vars())
                            if (Code())
                                if (match(LexicalUnit.END)) return true;
    }

    syntaxError(r);
    return false;
}

boolean Vars() {
    int r;
    switch (tok) {
        case VARNAME:
        case IF:
        case DO:
        case PRINT:
        case READ:
        case END:
            r = 3;
            printRule(r);
            return true;

        case INTEGER:
            r = 2;
            printRule(r);

            if (match(LexicalUnit.INTEGER))
                if (VarList())
                    if (match(LexicalUnit.ENDLINE)) return true;

            syntaxError(r);
            break;
    }

    return false;
}

boolean VarList() {
    int r;
    switch (tok) {
        case VARNAME:
            r = 4;
            printRule(r);

            if (match(LexicalUnit.VARNAME))
                if (VarList_next()) return true;

            syntaxError(r);
            break;
    }

    return false;
}

boolean VarList_next() {
    int r;
    switch (tok) {
        case ENDLINE:
            r = 6;
            printRule(r);
            return true;

        case COMMA:
            r = 5;
            printRule(r);

            if (match(LexicalUnit.COMMA))
                if (VarList()) return true;

            syntaxError(r);
            break;
    }

    return false;
}

boolean Code() {
    int r;
    switch (tok) {
        case END:
        case ENDIF:
        case ELSE:
        case ENDDO:
            r = 8;
            printRule(r);
            return true;

        case VARNAME:
        case IF:
        case DO:
        case PRINT:
        case READ:
            r = 7;
            printRule(r);

            if (Instruction())
                if (match(LexicalUnit.ENDLINE))
                    if (Code()) return true;

            syntaxError(r);
            break;
    }

    return false;
}

boolean Instruction() {
    int r;
    switch (tok) {
        case VARNAME:
            r = 9;
            printRule(r);

            if (Assign()) return true;

            syntaxError(r);
            break;

        case IF:
            r = 10;
            printRule(r);

            if (If()) return true;

            syntaxError(r);
            break;

        case DO:
            r = 11;
            printRule(r);

            if (Do()) return true;

            syntaxError(r);
            break;

        case PRINT:
            r = 12;
            printRule(r);

            if (Print()) return true;

            syntaxError(r);
            break;

        case READ:
            r = 13;
            printRule(r);

            if (Read()) return true;

            syntaxError(r);
            break;
    }

    return false;
}

boolean Assign() {
    int r;
    switch (tok) {
        case VARNAME:
            r = 14;
            printRule(r);

            if (match(LexicalUnit.VARNAME))
                if (match(LexicalUnit.EQUAL))
                    if (ExprArith()) return true;

            syntaxError(r);
            break;
    }

    return false;
}

boolean ExprArith() {
    int r;
    switch (tok) {
        case VARNAME:
        case NUMBER:
        case LEFT_PARENTHESIS:
        case MINUS:
            r = 15;
            printRule(r);

            if (ExprArith_b())
                if (ExprArithP()) return true;

            syntaxError(r);
            break;
    }

    return false;
}

boolean ExprArithP() {
    int r;
    switch (tok) {
        case RIGHT_PARENTHESIS:
        case AND:
        case OR:
        case EQUAL_COMPARE:
        case GREATER_EQUAL:
        case GREATER:
        case SMALLER_EQUAL:
        case SMALLER:
        case DIFFERENT:
        case ENDLINE:
            r = 17;
            printRule(r);
            return true;

        case MINUS:
        case PLUS:
            r = 16;
            printRule(r);

            if (OpPlusMinus())
                if (ExprArith_b())
                    if (ExprArithP()) return true;

            syntaxError(r);
            break;
    }

    return false;
}

boolean ExprArith_b() {
    int r;
    switch (tok) {
        case VARNAME:
        case NUMBER:
        case LEFT_PARENTHESIS:
        case MINUS:
            r = 18;
            printRule(r);

            if (ExprArith_c())
                if (ExprArith_bP()) return true;

            syntaxError(r);
            break;
    }

    return false;
}

boolean ExprArith_bP() {
    int r;
    switch (tok) {
        case RIGHT_PARENTHESIS:
        case MINUS:
        case PLUS:
        case AND:
        case OR:
        case EQUAL_COMPARE:
        case GREATER_EQUAL:
        case GREATER:
        case SMALLER_EQUAL:
        case SMALLER:
        case DIFFERENT:
        case ENDLINE:
            r = 20;
            printRule(r);
            return true;

        case TIMES:
        case DIVIDE:
            r = 19;
            printRule(r);

            if (OpTimesDivide())
                if (ExprArith_c())
                    if (ExprArith_bP()) return true;

            syntaxError(r);
            break;
    }

    return false;
}

boolean ExprArith_c() {
    int r;
    switch (tok) {
        case VARNAME:
            r = 22;
            printRule(r);

            if (match(LexicalUnit.VARNAME)) return true;

            syntaxError(r);
            break;


        case NUMBER:
            r = 23;
            printRule(r);

            if (match(LexicalUnit.NUMBER)) return true;

            syntaxError(r);
            break;

        case LEFT_PARENTHESIS:
            r = 24;
            printRule(r);

            if (match(LexicalUnit.LEFT_PARENTHESIS))
                if (ExprArith())
                    if (match(LexicalUnit.RIGHT_PARENTHESIS)) return true;

            syntaxError(r);
            break;

        case MINUS:
            r = 21;
            printRule(r);

            if (match(LexicalUnit.MINUS))
                if (ExprArith_c()) return true;

            syntaxError(r);
            break;
    }

    return false;
}

boolean OpPlusMinus() {
    int r;
    switch (tok) {
        case MINUS:
            r = 26;
            printRule(r);

            if (match(LexicalUnit.MINUS)) return true;

            syntaxError(r);
            break;

        case PLUS:
            r = 25;
            printRule(r);

            if (match(LexicalUnit.PLUS)) return true;

            syntaxError(r);
            break;
    }

    return false;
}

boolean OpTimesDivide() {
    int r;
    switch (tok) {
        case TIMES:
            r = 27;
            printRule(r);

            if (match(LexicalUnit.TIMES)) return true;

            syntaxError(r);
            break;

        case DIVIDE:
            r = 28;
            printRule(r);

            if (match(LexicalUnit.DIVIDE)) return true;

            syntaxError(r);
            break;
    }

    return false;
}

boolean If() {
    int r;
    switch (tok) {
        case IF:
            r = 29;
            printRule(r);

            if (match(LexicalUnit.IF))
                if (match(LexicalUnit.LEFT_PARENTHESIS))
                    if (Cond())
                        if (match(LexicalUnit.RIGHT_PARENTHESIS))
                            if (match(LexicalUnit.THEN))
                                if (match(LexicalUnit.ENDLINE))
                                    if (Code())
                                        if (If_next()) return true;

            syntaxError(r);
            break;
    }

    return false;
}

boolean If_next() {
    int r;
    switch (tok) {
        case ENDIF:
            r = 30;
            printRule(r);

            if (match(LexicalUnit.ENDIF)) return true;

            syntaxError(r);
            break;

        case ELSE:
            r = 31;
            printRule(r);

            if (match(LexicalUnit.ELSE))
                if (match(LexicalUnit.ENDLINE))
                    if (Code())
                        if (match(LexicalUnit.ENDIF)) return true;

            syntaxError(r);
            break;
    }

    return false;
}

boolean Cond() {
    int r;
    switch (tok) {
        case VARNAME:
        case NUMBER:
        case LEFT_PARENTHESIS:
        case MINUS:
        case NOT:
            r = 32;
            printRule(r);

            if (Cond_b())
                if (CondP()) return true;

            syntaxError(r);
            break;
    }

    return false;
}

boolean CondP() {
    int r;
    switch (tok) {
        case RIGHT_PARENTHESIS:
        case MINUS:
        case NOT:
            r = 34;
            printRule(r);
            return true;

        case OR:
            r = 33;
            printRule(r);

            if (match(LexicalUnit.OR))
                if (Cond_b())
                    if (CondP()) return true;

            syntaxError(r);
            break;
    }

    return false;
}

boolean Cond_b() {
    int r;
    switch (tok) {
        case VARNAME:
        case NUMBER:
        case LEFT_PARENTHESIS:
        case MINUS:
        case NOT:
            r = 35;
            printRule(r);

            if (Cond_c())
                if (Cond_bP()) return true;

            syntaxError(r);
            break;
    }

    return false;
}

boolean Cond_bP() {
    int r;
    switch (tok) {
        case RIGHT_PARENTHESIS:
        case OR:
            r = 37;
            printRule(r);
            return true;

        case AND:
            r = 36;
            printRule(r);

            if (match(LexicalUnit.AND))
                if (Cond_c())
                    if (Cond_bP()) return true;

            syntaxError(r);
            break;
    }

    return false;
}

boolean Cond_c() {
    int r;
    switch (tok) {
        case VARNAME:
        case NUMBER:
        case LEFT_PARENTHESIS:
        case MINUS:
            r = 39;
            printRule(r);

            if (SimpleCond()) return true;

            syntaxError(r);
            break;

        case NOT:
            r = 38;
            printRule(r);

            if (match(LexicalUnit.NOT))
                if (SimpleCond()) return true;

            syntaxError(r);
            break;
    }

    return false;
}

boolean SimpleCond() {
    int r;
    switch (tok) {
        case VARNAME:
        case NUMBER:
        case LEFT_PARENTHESIS:
        case MINUS:
            r = 40;
            printRule(r);

            if (ExprArith())
                if (Comp())
                    if (ExprArith()) return true;

            syntaxError(r);
            break;
    }

    return false;
}

boolean Comp() {
    int r;
    switch (tok) {
        case EQUAL_COMPARE:
            r = 41;
            printRule(r);

            if (match(LexicalUnit.EQUAL_COMPARE)) return true;

            syntaxError(r);
            break;

        case GREATER_EQUAL:
            r = 42;
            printRule(r);

            if (match(LexicalUnit.GREATER_EQUAL)) return true;

            syntaxError(r);
            break;

        case GREATER:
            r = 43;
            printRule(r);

            if (match(LexicalUnit.GREATER)) return true;

            syntaxError(r);
            break;

        case SMALLER_EQUAL:
            r = 44;
            printRule(r);

            if (match(LexicalUnit.SMALLER_EQUAL)) return true;

            syntaxError(r);
            break;

        case SMALLER:
            r = 45;
            printRule(r);

            if (match(LexicalUnit.SMALLER)) return true;

            syntaxError(r);
            break;

        case DIFFERENT:
            r = 46;
            printRule(r);

            if (match(LexicalUnit.DIFFERENT)) return true;

            syntaxError(r);
            break;
    }

    return false;
}

boolean Do() {
    int r;
    switch (tok) {
        case DO:
            r = 47;
            printRule(r);

            if (match(LexicalUnit.DO))
                if (match(LexicalUnit.VARNAME))
                    if (match(LexicalUnit.EQUAL))
                        if (match(LexicalUnit.NUMBER))
                            if (match(LexicalUnit.COMMA))
                                if (match(LexicalUnit.NUMBER))
                                    if (match(LexicalUnit.ENDLINE))
                                        if (Code())
                                            if (match(LexicalUnit.ENDDO)) return true;

            syntaxError(r);
            break;
    }

    return false;
}

boolean Print() {
    int r;
    switch (tok) {
        case PRINT:
            r = 48;
            printRule(r);

            if (match(LexicalUnit.PRINT))
                if (match(LexicalUnit.COMMA))
                    if (ExpList()) return true;

            syntaxError(r);
            break;
    }

    return false;
}

boolean Read() {
    int r;
    switch (tok) {
        case READ:
            r = 49;
            printRule(r);

            if (match(LexicalUnit.READ))
                if (match(LexicalUnit.COMMA))
                    if (VarList()) return true;

            syntaxError(r);
            break;
    }

    return false;
}

boolean ExpList() {
    int r;
    switch (tok) {
        case VARNAME:
        case NUMBER:
        case LEFT_PARENTHESIS:
        case MINUS:
            r = 50;
            printRule(r);

            if (ExprArith())
                if (ExpList_next()) return true;

            syntaxError(r);
            break;
    }

    return false;
}

boolean ExpList_next() {
    int r;
    switch (tok) {
        case ENDLINE:
            r = 52;
            printRule(r);
            return true;

        case COMMA:
            r = 51;
            printRule(r);

            if (match(LexicalUnit.COMMA))
                if (ExpList()) return true;

            syntaxError(r);
            break;
    }

    return false;
}

void syntaxError(int i) {
    System.out.println("A syntax error occurs at rule [" + i + "]");
}

void printRule(int i) {
    String toPrint = "";
    switch (i) {
        case 1:
            toPrint = "[" + i + "] " + "<Program> \t\t--> PROGRAM [ProgName] [EndLine] <Vars> <Code> END";
            break;

        case 2:
            toPrint = "[" + i + "] " + "<Vars> \t\t--> INTEGER <VarList> [EndLine]";
            break;

        case 3:
            toPrint = "[" + i + "] " + "<Vars> \t\t--> e";
            break;

        case 4:
            toPrint = "[" + i + "] " + "<VarList> \t\t--> [VarName] <VarList-next>";
            break;

        case 5:
            toPrint = "[" + i + "] " + "<VarList-next> \t--> , <VarList>";
            break;

        case 6:
            toPrint = "[" + i + "] " + "<VarList-next> \t--> e";
            break;

        case 7:
            toPrint = "[" + i + "] " + "<Code> \t\t--> <Instruction> [EndLine] <Code>";
            break;

        case 8:
            toPrint = "[" + i + "] " + "<Code> \t\t--> e";
            break;

        case 9:
            toPrint = "[" + i + "] " + "<Instruction> \t--> <Assign>";
            break;

        case 10:
            toPrint = "[" + i + "] " + "<Instruction> \t--> <If>";
            break;

        case 11:
            toPrint = "[" + i + "] " + "<Instruction> \t--> <Do>";
            break;

        case 12:
            toPrint = "[" + i + "] " + "<Instruction> \t--> <Print>";
            break;

        case 13:
            toPrint = "[" + i + "] " + "<Instruction> \t--> <Read>";
            break;

        case 14:
            toPrint = "[" + i + "] " + "<Assign> \t\t--> [VarName] = <ExprArith>";
            break;

        case 15:
            toPrint = "[" + i + "] " + "<ExprArith> \t--> <ExprArith_b> <ExprArith'>";
            break;

        case 16:
            toPrint = "[" + i + "] " + "<ExprArith'> \t--> <Op+-> <ExprArith_b> <ExprArith'>";
            break;

        case 17:
            toPrint = "[" + i + "] " + "<ExprArith'> \t--> e";
            break;

        case 18:
            toPrint = "[" + i + "] " + "<ExprArith_b> \t--> <ExprArith_c> <ExprArith_b'>";
            break;

        case 19:
            toPrint = "[" + i + "] " + "<ExprArith_b'> \t--> <Op*/> <ExprArith_c> <ExprArith_b'>";
            break;

        case 20:
            toPrint = "[" + i + "] " + "<ExprArith_b'> \t--> e";
            break;

        case 21:
            toPrint = "[" + i + "] " + "<ExprArith_c> \t--> - <ExprArith_c>";
            break;

        case 22:
            toPrint = "[" + i + "] " + "<ExprArith_c> \t--> [VarName]";
            break;

        case 23:
            toPrint = "[" + i + "] " + "<ExprArith_c> \t--> [Number]";
            break;

        case 24:
            toPrint = "[" + i + "] " + "<ExprArith_c> \t--> ( <ExprArith> )";
            break;

        case 25:
            toPrint = "[" + i + "] " + "<Op+-> \t\t--> +";
            break;

        case 26:
            toPrint = "[" + i + "] " + "<Op+-> \t\t--> -";
            break;

        case 27:
            toPrint = "[" + i + "] " + "<Op*/> \t\t--> *";
            break;

        case 28:
            toPrint = "[" + i + "] " + "<Op*/> \t\t--> /";
            break;

        case 29:
            toPrint = "[" + i + "] " + "<If> \t\t--> IF (<Cond>) THEN [EndLine] <Code> <If-next>";
            break;

        case 30:
            toPrint = "[" + i + "] " + "<If-next> \t\t--> ENDIF";
            break;

        case 31:
            toPrint = "[" + i + "] " + "<If-next> \t\t--> ELSE [EndLine] <Code> ENDIF";
            break;

        case 32:
            toPrint = "[" + i + "] " + "<Cond> \t\t--> <Cond_b> <Cond'>";
            break;

        case 33:
            toPrint = "[" + i + "] " + "<Cond'> \t\t--> .OR. <Cond_b> <Cond'>";
            break;

        case 34:
            toPrint = "[" + i + "] " + "<Cond'> \t\t--> e";
            break;

        case 35:
            toPrint = "[" + i + "] " + "<Cond_b> \t\t--> <Cond_c> <Cond_b'>";
            break;

        case 36:
            toPrint = "[" + i + "] " + "<Cond_b'> \t\t--> .AND. <Cond_c> <Cond_b'>";
            break;

        case 37:
            toPrint = "[" + i + "] " + "<Cond_b'> \t\t--> e";
            break;

        case 38:
            toPrint = "[" + i + "] " + "<Cond_c> \t\t--> .NOT. <SimpleCond>";
            break;

        case 39:
            toPrint = "[" + i + "] " + "<Cond_c> \t\t--> <SimpleCond>";
            break;

        case 40:
            toPrint = "[" + i + "] " + "<SimpleCond> \t--> <ExprArith> <Comp> <ExprArith>";
            break;

        case 41:
            toPrint = "[" + i + "] " + "<Comp> \t\t--> .EQ.";
            break;

        case 42:
            toPrint = "[" + i + "] " + "<Comp> \t\t--> .GE.";
            break;

        case 43:
            toPrint = "[" + i + "] " + "<Comp> \t\t--> .GT.";
            break;

        case 44:
            toPrint = "[" + i + "] " + "<Comp> \t\t--> .LE.";
            break;

        case 45:
            toPrint = "[" + i + "] " + "<Comp> \t\t--> .LT.";
            break;

        case 46:
            toPrint = "[" + i + "] " + "<Comp> \t\t--> .NE.";
            break;

        case 47:
            toPrint = "[" + i + "] " + "<Do> \t\t--> DO [VarName] = [Number], [Number] [EndLine] <Code> ENDDO";
            break;

        case 48:
            toPrint = "[" + i + "] " + "<Print> \t\t--> PRINT*, <ExpList>";
            break;

        case 49:
            toPrint = "[" + i + "] " + "<Read> \t\t--> READ*, <VarList>";
            break;

        case 50:
            toPrint = "[" + i + "] " + "<ExpList> \t\t--> <ExprArith> <ExpList-next>";
            break;

        case 51:
            toPrint = "[" + i + "] " + "<ExpList-next> \t--> , <ExpList>";
            break;

        case 52:
            toPrint = "[" + i + "] " + "<ExpList-next> \t--> e";
            break;
    }
    System.out.println(toPrint);
}
}
