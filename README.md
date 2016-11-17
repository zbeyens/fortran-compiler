# fortran-compiler
(1a-1b done) Left-factoring:
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
[15] → [Number] <ExprArith'>
[16] → ( <ExprArith> ) <ExprArith'>
[17] → - <ExprArith> <ExprArith'>
[18] <ExprArith'> → <Op> <ExprArith> <ExprArith'>
[18] → ε

[19] <Op> → +
[20] → -
[21] → *
[22] → /

[23] <If> → IF (<Cond>) THEN [EndLine] <Code> <If-tail>
[24] <If-tail> → ENDIF
[25] → ELSE [EndLine] <Code> ENDIF

[26] <Cond> → .NOT. <SimpleCond> <Cond'>
[27] → <SimpleCond> <Cond'>
[25] <Cond'> → <BinOp> <Cond> <Cond'>
[25] → ε

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

____

(1c to finish) non-ambiguous:

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

#???
[14] <ExprArith> → [VarName] <ExprArith'>
[15] → [Number]   <ExprArith'>
[16] → ( <ExprArith> ) <ExprArith'>
[  ] → <ExprArith 1>
[17] <ExprArith 1> → - <ExprArith> <ExprArith'>
[18] <ExprArith'> → <Op+-> <ExprArith> <ExprArith' bis>
[  ] → <ExprArith' bis>
[  ] <ExprArith' bis> → <Op*/> <ExprArith>
[18] → ε

[19] <Op+-> → +
[20] -> → -

[21] <Op*/> → *
[22] → /

[23] <If> → IF (<Cond>) THEN [EndLine] <Code> <If-tail>
[24] <If-tail> → ENDIF
[25] → ELSE [EndLine] <Code> ENDIF

[26] <Cond> → .NOT. <SimpleCond> <Cond'>
[27] → <SimpleCond> <Cond'>

[25] <Cond'> → <BinOpOr> <Cond> <Cond' bis>
[  ] → <Cond' bis>
[  ] <Cond' bis> → <BinOpAnd> <Cond> <Cond'>
[25] → ε

[28] <SimpleCond> → <ExprArith> <Comp> <ExprArith>
[29] <BinOpAnd> → .AND.
[30] <BinOpOr> → .OR.
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
