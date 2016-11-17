# fortran-compiler
Left-factoring:
[1] <Program> → PROGRAM [ProgName] [EndLine] <Vars> <Code> END
[2] <Vars> → INTEGER <VarList> [EndLine]
[3] → ε
[4] <VarList> → [VarName] <VarList-tail>
[5] <VarList-tail> → , <VarList>
[6] → ε


[6] <Code> → <Instruction> [EndLine] <Code>
[7] → ε
[8] <Instruction> → <Assign>
[9] → <If>
[10] → <Do>
[11] → <Print>
[12] → <Read>
[13] <Assign> → [VarName] = <ExprArith>

[14] <ExprArith> → [VarName] <ExprArith'>
[15] → [Number] <ExprArith> <ExprArith'>
[16] → ( <ExprArith> ) <ExprArith'>
[17] → - <ExprArith> <ExprArith'>
[18] <ExprArith'> → <Op> <ExprArith> <ExprArith'> | ε

[19] <Op> → +
[20] → -
[21] → *
[22] → /

[23] <If> → IF (<Cond>) THEN [EndLine] <Code> <If-tail>
[24] <If-tail> → ENDIF
[25] → ELSE [EndLine] <Code> ENDIF

[25] <Cond'> → <BinOp> <Cond> <Cond'> | ε
[26] <Cond> → .NOT. <SimpleCond> <Cond'>
[27] → <SimpleCond> <Cond'>

[28] <SimpleCond> → <ExprArith> <Comp> <ExprArith>
[29] <BinOp> → .AND.
[30] → .OR.
[31] <Comp> → .EQ.
[32] → .GE.
[33] → .GT.
[34] → .LE.
[35] → .LT.
[36] → .NE.
[37] <Do> → DO [VarName] = [Number], [Number] [EndLine] <Code> ENDDO
[38] <Print> → PRINT*, <ExpList>
[39] <Read> → READ*, <VarList>

[40] <ExpList> → <ExprArith> <ExpList-tail>
[41] <ExpList-tail> → , <ExpList>
[42] → ε
