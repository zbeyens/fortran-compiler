import java.util.*;
import java.io.*;
import java.util.HashMap;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;

public class Parser {
private ArrayList<Integer> ruleList;
private ArrayList<String> codeList;
private ArrayList<Symbol> symList;
private LexicalUnit tok;
private String tokValue;
private int curTok;
private String syntaxError;
private String label;
private String state;
private String fileNameOut;

private HashMap<String, Integer> variables;
private ArrayList<String> condList; //stack the if labels
private Integer temp; //To store temporary LLVM variables (%0, %1, etc.)
private Integer counter;
private Integer labelCounter; //counter for label
private Object leftExp;
private String condFalse;
private Object notCond1;
private LexicalUnit notCondOp;
private Object notCond2;

public Parser(String fileNameOut) {
	this.fileNameOut = fileNameOut;
    this.symList = LexicalAnalyzer.symList;
	
    curTok = 0;
    syntaxError = "";

    ruleList = new ArrayList<Integer>();

    codeList = new ArrayList<String>();
    variables = new HashMap<String, Integer>();
    condList = new ArrayList<String>();

    label = "";
    state = "";
    temp = -1;
    counter = -1;
    labelCounter = -1;
}

/**
 * Writes a string in the LLVM file with the default indentation level (1)
 * @param s line to write
 */
public void write(String s) {
    write(s, 1);
}

/**
 * Writes a string in the LLVM file with a certain indentation level
 * @param s line to write
 * @param indentationLevel indentation level
 */
public void write(String s, Integer indentationLevel) {
    String indentation = getIndentation(indentationLevel);
    codeList.add(indentation + s + "\n");
    // try {
    //     // fileWriter.write(indentation+s+"\n");
    // } catch (IOException e) {
    //     System.out.print(e);
    // }
}

/**
 * Given an indentation level, constructions the indentation string.
 * Indentation = indentation level * "    " (4 spaces)
 * @param indentationLevel indentation level
 * @return indentation string
 */
private String getIndentation(Integer indentationLevel) {
    String indentation = "";

    for (int i = 0; i < indentationLevel; i++) {
        indentation += "    ";
    }

    return indentation;
}

public void llvm_init() {
    write("declare i32 @getchar()", 0);
    write("declare void @putchar(i32 %n)\n", 0);

    //@readIntRec
    write("define i32 @readIntRec(i32 %acc) {", 0);
    write("%x = call i32 @getchar()");
    write("%greater = icmp sgt i32 %x, 47");
    write("br i1 %greater, label %above_0, label %last_character");
    write("above_0:", 0);
    write("%less_than = icmp slt i32 %x, 58");
    write("br i1 %less_than, label %below_9, label %last_character");
    write("below_9:", 0);
    write("%acc1 = mul i32 %acc,10");
    write("%x1 = sub i32 %x, 48");
    write("%acc2 = add i32 %acc1,%x1");
    write("%ret_val = call i32 @readIntRec(i32 %acc2)");
    write("ret i32 %ret_val");
    write("last_character:", 0);
    write("ret i32 %acc");
    write("}\n", 0);

    //@readInt
    write("define i32 @readInt() {", 0);
    write("entry:", 0);
    write("%ret_val = call i32 @readIntRec(i32 0)");
    write("ret i32 %ret_val");
    write("}\n", 0);

    //@printIntRec
    write("define void @printIntRec(i32 %n) {", 0);
    write("%is_zero = icmp ne i32 %n, 0 ");
    write("br i1 %is_zero, label %print, label %do_nothing");
    write("do_nothing: ", 0);
    write("ret void");
    write("print:", 0);
    write("%one_tenth = sdiv i32 %n, 10");
    write("call void @printIntRec(i32 %one_tenth) ");
    write("%mod = srem i32 %n, 10");
    write("%ascii = add i32 %mod, 48");
    write("call void @putchar(i32 %ascii)");
    write("ret void");
    write("}\n", 0);

    //@printInt
    write("define void @printInt(i32 %n) {", 0);
    write("%is_zero = icmp ne i32 %n, 0");
    write("br i1 %is_zero, label %print_any_number, label %print_zero");
    write("print_any_number:", 0);
    write("call void @printIntRec(i32 %n)");
    write("br label %print_newline");
    write("print_zero:", 0);
    write("call void @putchar(i32 48)");
    write("br label %print_newline");
    write("print_newline:", 0);
    write("call void @putchar(i32 10)");
    write("ret void");
    write("}\n", 0);

    //@main
    write("define void @main() {", 0);
}

/**
 * Closes the LLVM file. Called when the LL(1) parsing is finished
 */
public void llvm_close() {
    write("ret void");
    write("}", 0);
    // try {
    //     // fileWriter.close();
    // } catch (IOException e) {
    //     System.out.print(e);
    // }
}

//Print/Read
public void printInt(Object res) {
    write("call void @printInt(i32 " + res + ")");
}

public void readInt(String varName) throws Exception {
    check(varName);

    String tmp = nextTemp();
    write("%" + tmp + " = call i32 @readInt()");
    write("store i32 %" + tmp + ", i32* " + varName);
}

//labels
public void nextLabel(String lab) throws Exception {
    label = lab;
    write(label + ":", 0);
}

//cond
public String icmp(Object left, LexicalUnit comp, Object right, String i) {
    String cond = "";

    switch (comp) {
        case EQUAL_COMPARE:
            cond = "eq";
            break;

        case GREATER_EQUAL:
            cond = "sge";
            break;

        case GREATER:
            cond = "sgt";
            break;

        case SMALLER_EQUAL:
            cond = "sle";
            break;

        case SMALLER:
            cond = "slt";
            break;

        case DIFFERENT:
            cond = "ne";
            break;
    }

    String newTemp = "%" + nextTemp();
    write(newTemp + " = icmp " + cond + " " + i + " " + left + ", " + right);
    return newTemp;
}

public String icmpNot(Object left, LexicalUnit comp, Object right, String i) {
    String cond = "";

    switch (comp) {
        case EQUAL_COMPARE:
            cond = "ne";
            break;

        case GREATER_EQUAL:
            cond = "slt";
            break;

        case GREATER:
            cond = "sle";
            break;

        case SMALLER_EQUAL:
            cond = "sgt";
            break;

        case SMALLER:
            cond = "sge";
            break;

        case DIFFERENT:
            cond = "eq";
            break;
    }

    String newTemp = "%" + nextTemp();
    write(newTemp + " = icmp " + cond + " " + i + " " + left + ", " + right);
    return newTemp;
}

//for
public void initFor(String varName, String value1, String doCounter) throws Exception {
    assign("%" + varName, value1);
    nextLabel("for_loop_" + doCounter);
}

public void forLoop(String varName, String value2, String doCounter) throws Exception {
    String newTemp1 = loadVariable(varName);
    String newTemp2 = icmp(new String(newTemp1), LexicalUnit.SMALLER_EQUAL, new String(value2), "i32");
    String inner_for = "inner_for_" + doCounter;
    String after_for = "after_for" + doCounter;
    write("br i1 " + newTemp2 + ", label %" + inner_for + ", label %" + after_for);
    nextLabel(inner_for);
}

public void endFor(String varName, String doCounter) throws Exception {
    String newTemp1 = loadVariable(varName);
    String newTemp2 = "%" + nextTemp();
    write(newTemp2 + " = add i32 " + newTemp1 + ", 1");
    assign("%" + varName, newTemp2);
    write("br label %for_loop_" + doCounter);

    nextLabel("after_for" + doCounter);
}

/*****************************************************************************************************************
******************************************** LLVM ASSIGN INSTRUCTION ********************************************
*****************************************************************************************************************/

public void createVariable(String varName) throws Exception {
    if (variables.containsKey(varName)) throw new Exception("Already declared " + varName);

    variables.put(varName, ++counter);
}

public String nextTemp() {
    return (++temp) + "";
}

public void check(String varName) throws Exception {
    if (!variables.containsKey(varName)) throw new Exception("Undeclared " + varName);
}

/**
 * Assigns a value to a varName
 * @param varName variable to which we'll assign the value
 */
public void assign(String varName, Object value) throws Exception {
    check(varName);

    write("store i32 " + value + ", i32* " + varName);
}

//ExprArith
public String loadVariable(String varName) throws Exception {
    check("%" + varName);
    String newTemp = "%" + nextTemp();
    write(newTemp + " = load i32* %" + varName);
    leftExp = newTemp;
    return newTemp;
}

public String exprOp(Object left, Object right, LexicalUnit op) throws Exception {
    String newTemp = "%" + nextTemp();

    switch (op) {
        case MINUS:
            write(newTemp + " = mul i32 -1, " + right);
            break;

        case PLUS:
            write(newTemp + " = add i32 " + left + ", " + right);
            break;

        case TIMES:
            write(newTemp + " = mul i32 " + left + ", " + right);
            break;

        case DIVIDE:
            write(newTemp + " = sdiv i32 " + left + ", " + right);
            break;
    }

    leftExp = newTemp;
    return newTemp;
}

/*****************************************************************************************************************
****************************************** PARSER  ***************************************************************
*****************************************************************************************************************/

public void parse() throws Exception {
    nextsym();
    program();
    printRules();
    generateCode();
}

void nextsym() {
    tok = symList.get(curTok).getType();
    tokValue = symList.get(curTok).getValue() + "";
    curTok++;
}

//accessors

boolean match(LexicalUnit t) {
    if (tok == t) {
        nextsym();
        return true;
    } else {
        return false;
    }
}

Object program() throws Exception {
    int r = 1;


    switch (tok) {
        case PROGRAM:
            addRule(r);

            if (match(LexicalUnit.PROGRAM)) {
                llvm_init();
                nextLabel("entry");

                if (match(LexicalUnit.VARNAME))
                    if (match(LexicalUnit.ENDLINE))
                        if (!(Vars() instanceof Boolean))
                            if (!(Code() instanceof Boolean))
                                if (match(LexicalUnit.END)) {
                                    llvm_close();
                                    return new String("");
                                }
            }
    }

    printSyntaxError(r);
    return new Boolean(false);
}

Object Vars() throws Exception {
    int r;
    switch (tok) {
        case VARNAME:
        case IF:
        case DO:
        case PRINT:
        case READ:
        case END:
            r = 3;
            addRule(r);
            return new String("");

        case INTEGER:
            r = 2;
            addRule(r);
            state = "vars";

            if (match(LexicalUnit.INTEGER))
                if (!(VarList() instanceof Boolean))
                    if (match(LexicalUnit.ENDLINE)) {
                        state = "code";
                        return new String("");
                    }

            printSyntaxError(r);
            break;
    }

    return new Boolean(false);
}

Object VarList() throws Exception {
    int r;
    switch (tok) {
        case VARNAME:
            r = 4;
            addRule(r);

            String value = "%" + tokValue;

            if (match(LexicalUnit.VARNAME)) {
                if (state == "vars") {
                    createVariable(value);
                    write(value + " = alloca i32");
                }

                if (state == "read") {
                    readInt(value);
                }

                if (!(VarList_next() instanceof Boolean)) return new String("");
            }

            printSyntaxError(r);
            break;
    }

    return new Boolean(false);
}

Object VarList_next() throws Exception {
    int r;
    switch (tok) {
        case ENDLINE:
            r = 6;
            addRule(r);
            return new String("");

        case COMMA:
            r = 5;
            addRule(r);

            if (match(LexicalUnit.COMMA))
                if (!(VarList() instanceof Boolean)) return new String("");

            printSyntaxError(r);
            break;
    }

    return new Boolean(false);
}

Object Code() throws Exception {
    int r;
    switch (tok) {
        case END:
        case ENDIF:
        case ELSE:
        case ENDDO:
            r = 8;
            addRule(r);
            return new String("");

        case VARNAME:
        case IF:
        case DO:
        case PRINT:
        case READ:
            r = 7;
            addRule(r);

            if (!(Instruction() instanceof Boolean))
                if (match(LexicalUnit.ENDLINE))
                    if (!(Code() instanceof Boolean)) return new String("");

            printSyntaxError(r);
            break;
    }

    return new Boolean(false);
}

Object Instruction() throws Exception {
    int r;
    switch (tok) {
        case VARNAME:
            r = 9;
            addRule(r);

            if (!(Assign() instanceof Boolean)) return new String("");

            printSyntaxError(r);
            break;

        case IF:
            r = 10;
            addRule(r);

            if (!(If() instanceof Boolean)) return new String("");

            printSyntaxError(r);
            break;

        case DO:
            r = 11;
            addRule(r);

            if (!(Do() instanceof Boolean)) return new String("");

            printSyntaxError(r);
            break;

        case PRINT:
            r = 12;
            addRule(r);

            if (!(Print() instanceof Boolean)) return new String("");

            printSyntaxError(r);
            break;

        case READ:
            r = 13;
            addRule(r);

            if (!(Read() instanceof Boolean)) return new String("");

            printSyntaxError(r);
            break;
    }

    return new Boolean(false);
}

Object Assign() throws Exception {
    int r;
    switch (tok) {
        case VARNAME:
            r = 14;
            addRule(r);

            String value = "%" + tokValue;
            Object res;

            if (match(LexicalUnit.VARNAME))
                if (match(LexicalUnit.EQUAL))
                    if (!((res = ExprArith()) instanceof Boolean)) {
                        assign(value, res);
                        return new String("");
                    }

            printSyntaxError(r);
            break;
    }

    return new Boolean(false);
}

Object ExprArith() throws Exception {
    int r;
    switch (tok) {
        case VARNAME:
        case NUMBER:
        case LEFT_PARENTHESIS:
        case MINUS:
            r = 15;
            addRule(r);

            Object res1;
            Object res2;

            if (!((res2 = ExprArith_b()) instanceof Boolean)) {
                if (!((res1 = ExprArithP()) instanceof Boolean)) {
                    if (res1.equals("")) return res2;
                    else return res1;
                }
            }

            printSyntaxError(r);
            break;
    }

    return new Boolean(false);
}

Object ExprArithP() throws Exception {
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
            addRule(r);
            return new String("");

        case MINUS:
        case PLUS:
            r = 16;
            addRule(r);

            LexicalUnit value = tok;
            Object res2;
            Object res1;
            Object left = leftExp;

            if (!(OpPlusMinus() instanceof Boolean)) {
                if (!((res2 = ExprArith_b()) instanceof Boolean)) {
                    String newTemp = exprOp(left, res2, value);

                    if (!((res1 = ExprArithP()) instanceof Boolean)) {
                        return newTemp;
                    }
                }
            }

            printSyntaxError(r);
            break;
    }

    return new Boolean(false);
}

Object ExprArith_b() throws Exception {
    int r;
    switch (tok) {
        case VARNAME:
        case NUMBER:
        case LEFT_PARENTHESIS:
        case MINUS:
            r = 18;
            addRule(r);

            Object res1;
            Object res2;

            if (!((res2 = ExprArith_c()) instanceof Boolean))
                if (!((res1 = ExprArith_bP()) instanceof Boolean)) {
                    if (res1.equals("")) return res2;
                    else return res1;
                }

            printSyntaxError(r);
            break;
    }

    return new Boolean(false);
}

Object ExprArith_bP() throws Exception {
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
            addRule(r);
            return new String("");

        case TIMES:
        case DIVIDE:
            r = 19;
            addRule(r);

            LexicalUnit value = tok;
            Object res1;
            Object res2;
            Object left = leftExp;

            if (!(OpTimesDivide() instanceof Boolean))
                if (!((res2 = ExprArith_c()) instanceof Boolean)) {
                    String newTemp = exprOp(left, res2, value);

                    if (!((res1 = ExprArith_bP()) instanceof Boolean)) {
                        return newTemp;
                    }
                }

            printSyntaxError(r);
            break;
    }


    return new Boolean(false);
}

Object ExprArith_c() throws Exception {
    int r;
    String value = tokValue;
    Object res;
    switch (tok) {
        case VARNAME:
            r = 22;
            addRule(r);

            if (match(LexicalUnit.VARNAME)) {
                return new String(loadVariable(value));
            }

            printSyntaxError(r);
            break;


        case NUMBER:
            r = 23;
            addRule(r);

            if (match(LexicalUnit.NUMBER)) {
                leftExp = new String(value);
                return leftExp;
            }

            printSyntaxError(r);
            break;

        case LEFT_PARENTHESIS:
            r = 24;
            addRule(r);

            if (match(LexicalUnit.LEFT_PARENTHESIS)) {
                if (!((res = ExprArith()) instanceof Boolean)) {
                    if (match(LexicalUnit.RIGHT_PARENTHESIS)) return res;
                }
            }

            printSyntaxError(r);
            break;

        case MINUS:
            r = 21;
            addRule(r);

            if (match(LexicalUnit.MINUS))
                if (!((res = ExprArith_c()) instanceof Boolean)) {
                    String newTemp = "%" + nextTemp();
                    write(newTemp + " = mul i32 -1, " + res);

                    leftExp = newTemp;
                    return new String(newTemp);
                }

            printSyntaxError(r);
            break;
    }

    return new Boolean(false);
}

Object OpPlusMinus() {
    int r;
    switch (tok) {
        case MINUS:
            r = 26;
            addRule(r);

            if (match(LexicalUnit.MINUS)) {
                return new String("");
            }

            printSyntaxError(r);
            break;

        case PLUS:
            r = 25;
            addRule(r);

            if (match(LexicalUnit.PLUS)) {
                return new String("");
            }

            printSyntaxError(r);
            break;
    }

    return new Boolean(false);
}

Object OpTimesDivide() {
    int r;
    switch (tok) {
        case TIMES:
            r = 27;
            addRule(r);

            if (match(LexicalUnit.TIMES)) {
                return new String("");
            }

            printSyntaxError(r);
            break;

        case DIVIDE:
            r = 28;
            addRule(r);

            String value = tokValue;

            if (match(LexicalUnit.DIVIDE)) {
                return new String("");
            }

            printSyntaxError(r);
            break;
    }

    return new Boolean(false);
}

Object If() throws Exception {
    int r;
    switch (tok) {
        case IF:
            r = 29;
            addRule(r);

            Object res;
            String if_st = "if_" + ++labelCounter;
            condFalse = "after_if_" + ++labelCounter;

            if (match(LexicalUnit.IF))
                if (match(LexicalUnit.LEFT_PARENTHESIS))
                    if (!((res = Cond()) instanceof Boolean)) {
                        write("br i1 " + res + ", label %" + if_st + ", label %" + condFalse);
                        // %9 = icmp slt i32 %8, %nombre

                        if (match(LexicalUnit.RIGHT_PARENTHESIS))
                            if (match(LexicalUnit.THEN))
                                if (match(LexicalUnit.ENDLINE)) {
                                    nextLabel(if_st);

                                    if (!(Code() instanceof Boolean)) {
                                        write("br label %" + condFalse);
                                        nextLabel(condFalse);

                                        if (!(If_next() instanceof Boolean)) return new String("");
                                    }
                                }
                    }

            printSyntaxError(r);
            break;
    }

    return new Boolean(false);
}

Object If_next() throws Exception {
    int r;
    String after_if_else = "after_if_else_" + ++labelCounter;
    switch (tok) {
        case ENDIF:
            r = 30;
            addRule(r);

            if (match(LexicalUnit.ENDIF)) {
                write("br label %" + after_if_else);
                nextLabel(after_if_else);
                return new String("");
            }

            printSyntaxError(r);
            break;

        case ELSE:
            r = 31;
            addRule(r);

            if (match(LexicalUnit.ELSE)) {
                String else_st = "else_" + ++labelCounter;
                write("br label %" + else_st);
                nextLabel(else_st);

                if (match(LexicalUnit.ENDLINE))
                    if (!(Code() instanceof Boolean))
                        if (match(LexicalUnit.ENDIF)) {
                            write("br label %" + after_if_else);
                            nextLabel(after_if_else);
                            return new String("");
                        }
            }

            printSyntaxError(r);
            break;
    }

    return new Boolean(false);
}

Object Cond() throws Exception {
    int r;
    switch (tok) {
        case VARNAME:
        case NUMBER:
        case LEFT_PARENTHESIS:
        case MINUS:
        case NOT:
            r = 32;
            addRule(r);

            Object res1, res2;

            if (!((res1 = Cond_b()) instanceof Boolean))
                if (!((res2 = CondP(res1)) instanceof Boolean)) {
                    if (res2 != "") return res2;

                    return res1;
                }

            printSyntaxError(r);
            break;
    }

    return new Boolean(false);
}

Object CondP(Object res1) throws Exception {
    int r;
    switch (tok) {
        case RIGHT_PARENTHESIS:
        case MINUS:
        case NOT:
            r = 34;
            addRule(r);
            return new String("");

        case OR:
            r = 33;
            addRule(r);

            Object res2;

            if (match(LexicalUnit.OR))
                if (!((res2 = Cond_b()) instanceof Boolean)) {
                    String newTemp = "%" + nextTemp();
                    write(newTemp + " = or i1 " + res1 + ", " + res2);

                    if (!(CondP(newTemp) instanceof Boolean)) return new String(newTemp);
                }

            printSyntaxError(r);
            break;
    }

    return new Boolean(false);
}

Object Cond_b() throws Exception {
    int r;
    switch (tok) {
        case VARNAME:
        case NUMBER:
        case LEFT_PARENTHESIS:
        case MINUS:
        case NOT:
            r = 35;
            addRule(r);

            Object res1, res2;

            if (!((res1 = Cond_c()) instanceof Boolean))
                if (!((res2 = Cond_bP(res1)) instanceof Boolean)) {
                    if (res2 != "") return res2;

                    return res1;
                }

            printSyntaxError(r);
            break;
    }

    return new Boolean(false);
}

Object Cond_bP(Object res1) throws Exception {
    int r;
    switch (tok) {
        case RIGHT_PARENTHESIS:
        case OR:
            r = 37;
            addRule(r);
            return new String("");

        case AND:
            r = 36;
            addRule(r);

            Object res2;

            if (match(LexicalUnit.AND))
                if (!((res2 = Cond_c()) instanceof Boolean)) {
                    String newTemp = "%" + nextTemp();
                    write(newTemp + " = and i1 " + res1 + ", " + res2);

                    if (!(Cond_bP(newTemp) instanceof Boolean)) return new String(newTemp);
                }

            printSyntaxError(r);
            break;
    }

    return new Boolean(false);
}

Object Cond_c() throws Exception {
    int r;
    Object res;
    switch (tok) {
        case VARNAME:
        case NUMBER:
        case LEFT_PARENTHESIS:
        case MINUS:
            r = 39;
            addRule(r);

            if (!((res = SimpleCond()) instanceof Boolean)) {
                return new String(icmp(notCond1, notCondOp, notCond2, "i32"));
            }

            printSyntaxError(r);
            break;

        case NOT:
            r = 38;
            addRule(r);

            if (match(LexicalUnit.NOT))
                if (!((res = SimpleCond()) instanceof Boolean)) {
                    return new String(icmpNot(notCond1, notCondOp, notCond2, "i32"));
                }

            printSyntaxError(r);
            break;
    }

    return new Boolean(false);
}

Object SimpleCond() throws Exception {
    int r;
    switch (tok) {
        case VARNAME:
        case NUMBER:
        case LEFT_PARENTHESIS:
        case MINUS:
            r = 40;
            addRule(r);

            if (!((notCond1 = ExprArith()) instanceof Boolean)) {
                notCondOp = tok;

                if (!(Comp() instanceof Boolean))
                    if (!((notCond2 = ExprArith()) instanceof Boolean)) {
                        return new String("");
                    }
            }

            printSyntaxError(r);
            break;
    }

    return new Boolean(false);
}

Object Comp() throws Exception {
    int r;
    switch (tok) {
        case EQUAL_COMPARE:
            r = 41;
            addRule(r);

            if (match(LexicalUnit.EQUAL_COMPARE)) return new String("");

            printSyntaxError(r);
            break;

        case GREATER_EQUAL:
            r = 42;
            addRule(r);

            if (match(LexicalUnit.GREATER_EQUAL)) return new String("");

            printSyntaxError(r);
            break;

        case GREATER:
            r = 43;
            addRule(r);

            if (match(LexicalUnit.GREATER)) return new String("");

            printSyntaxError(r);
            break;

        case SMALLER_EQUAL:
            r = 44;
            addRule(r);

            if (match(LexicalUnit.SMALLER_EQUAL)) return new String("");

            printSyntaxError(r);
            break;

        case SMALLER:
            r = 45;
            addRule(r);

            if (match(LexicalUnit.SMALLER)) return new String("");

            printSyntaxError(r);
            break;

        case DIFFERENT:
            r = 46;
            addRule(r);

            if (match(LexicalUnit.DIFFERENT)) return new String("");

            printSyntaxError(r);
            break;
    }

    return new Boolean(false);
}

Object Do() throws Exception {
    int r;
    switch (tok) {
        case DO:
            r = 47;
            addRule(r);

            if (match(LexicalUnit.DO)) {
                String varName = tokValue;
                String doCounter = "" + ++labelCounter;

                if (match(LexicalUnit.VARNAME))
                    if (match(LexicalUnit.EQUAL)) {
                        String value1 = tokValue;

                        if (match(LexicalUnit.NUMBER))
                            if (match(LexicalUnit.COMMA)) {
                                String value2 = tokValue;

                                if (match(LexicalUnit.NUMBER))
                                    if (match(LexicalUnit.ENDLINE)) {
                                        initFor(varName, value1, doCounter);
                                        forLoop(varName, value2, doCounter);

                                        if (!(Code() instanceof Boolean)) {
                                            endFor(varName, doCounter);

                                            if (match(LexicalUnit.ENDDO)) return new String("");
                                        }
                                    }
                            }
                    }
            }

            printSyntaxError(r);
            break;
    }

    return new Boolean(false);
}

Object Print() throws Exception {
    int r;
    switch (tok) {
        case PRINT:
            r = 48;
            addRule(r);

            if (match(LexicalUnit.PRINT))
                if (match(LexicalUnit.COMMA)) {
                    state = "print";

                    if (!(ExpList() instanceof Boolean)) return new String("");
                }

            printSyntaxError(r);
            break;
    }

    return new Boolean(false);
}

Object Read() throws Exception {
    int r;
    switch (tok) {
        case READ:
            r = 49;
            addRule(r);

            if (match(LexicalUnit.READ))
                if (match(LexicalUnit.COMMA)) {
                    state = "read";

                    if (!(VarList() instanceof Boolean)) return new String("");
                }

            printSyntaxError(r);
            break;
    }

    return new Boolean(false);
}

Object ExpList() throws Exception {
    int r;
    switch (tok) {
        case VARNAME:
        case NUMBER:
        case LEFT_PARENTHESIS:
        case MINUS:
            r = 50;
            addRule(r);

            Object res;

            if (!((res = ExprArith()) instanceof Boolean)) {
                printInt(res);

                if (!(ExpList_next() instanceof Boolean)) return new String("");
            }

            printSyntaxError(r);
            break;
    }

    return new Boolean(false);
}

Object ExpList_next() throws Exception {
    int r;
    switch (tok) {
        case ENDLINE:
            r = 52;
            addRule(r);
            return new String("");

        case COMMA:
            r = 51;
            addRule(r);

            if (match(LexicalUnit.COMMA))
                if (!(ExpList() instanceof Boolean)) return new String("");

            printSyntaxError(r);
            break;
    }

    return new Boolean(false);
}

void printSyntaxError(int i) {
    if (syntaxError == "") {
        syntaxError = "A syntax error occurs at rule [" + i + "]";
    }
}

void addRule(int i) {
    ruleList.add(i);
}

void addCode(String s) {
    codeList.add(s);
}

void printRules() {
    for (int i = 0; i < ruleList.size(); i++) {
        // printRule(ruleList.get(i));
    }

    if (syntaxError != "") {
        System.out.println(syntaxError);
    }
}

void generateCode() {
	try{
		PrintWriter writer = new PrintWriter(fileNameOut, "UTF-8");
		if (syntaxError == "") {
			for (int i = 0; i < codeList.size(); i++) {
				writer.print(codeList.get(i));
			}
		}
		writer.close();
	} catch (IOException e) {
		System.out.println("error during creation of outputfile");
	}
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
