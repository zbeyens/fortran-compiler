# fortran-compiler

(1b) Left-recursion removal:

[18] <ExprArith> → <ExprArith-b> <ExprArith'>
     <ExprArith'> → <Op+-> <ExprArith-b> <ExprArith'>
     → ε
     <ExprArith-b> → <ExprArith-c> <ExprArith-b'>
     <ExprArith-b'> → <Op*/> <ExprArith-c> <ExprArith-b'>
     → ε
     <ExprArith-c> → - <ExprArith-c>
     →  [VarName]
     → [Number]
     → ( <ExprArith> )
[19] <Op+-> → +
[20] → -
[21] <Op*/> → *
[22] → /

[23] <If> → IF (<Cond>) THEN [EndLine] <Code> <If-next>
[24] <If-next> → ENDIF
[25] → ELSE [EndLine] <Code> ENDIF

[25] <Cond> → <Cond-b> <Cond'>
     <Cond'> → .OR. <Cond-b> <Cond'>
     → ε
[25] <Cond-b> → <Cond-c> <Cond-b'>
     <Cond-b'> → .AND. <Cond-c> <Cond-b'>
     → ε

     <Cond-c> → .NOT. <SimpleCond>
     → <SimpleCond>
[28] <SimpleCond> → <ExprArith> <Comp> <ExprArith>
[31] <Comp> → .EQ.
[32] → .GE.
[33] → .GT.
[34] → .LE.
[35] → .LT.
[36] → .NE.
[37] <Do> → DO [VarName] = [Number], [Number] [EndLine] <Code> ENDDO
[38] <Print> → PRINT*, <ExpList>
[39] <Read> → READ*, <VarList>

[40] <ExpList> → <ExprArith> <ExpList-next>
[41] <ExpList-next> → , <ExpList>
[42] → ε


(1b) Left-factoring:
[1] <Program> → PROGRAM [ProgName] [EndLine] <Vars> <Code> END
[2] <Vars> → INTEGER <VarList> [EndLine]
[3] → ε
[4] <VarList> → [VarName] <VarList-next>
[5] <VarList-next> → , <VarList>
[6] → ε

[6] <Code> → <Instruction> [EndLine] <Code>
[7] → ε
[8] <Instruction> → <Assign>
[9] → <If>
[10] → <Do>
[11] → <Print>
[12] → <Read>
[13] <Assign> → [VarName] = <ExprArith>

[18] <ExprArith> → <ExprArith> <Op+-> <ExprArith-b>
     → <ExprArith-b>
     <ExprArith-b> → <ExprArith-b> <Op*/> <ExprArith-c>
     → <ExprArith-c>
     <ExprArith-c> → - <ExprArith-c>
     →  [VarName]
     → [Number]
     → ( <ExprArith> )
[19] <Op+-> → +
[20] → -
[21] <Op*/> → *
[22] → /

[23] <If> → IF (<Cond>) THEN [EndLine] <Code> <If-next>
[24] <If-next> → ENDIF
[25] → ELSE [EndLine] <Code> ENDIF

[25] <Cond> → <Cond> .OR. <Cond-b>
     → <Cond-b>
[25] <Cond-b> → <Cond-b> .AND. <Cond-c>
     → <Cond-c>
     <Cond-c> → .NOT. <SimpleCond>
     → <SimpleCond>
[28] <SimpleCond> → <ExprArith> <Comp> <ExprArith>
[31] <Comp> → .EQ.
[32] → .GE.
[33] → .GT.
[34] → .LE.
[35] → .LT.
[36] → .NE.
[37] <Do> → DO [VarName] = [Number], [Number] [EndLine] <Code> ENDDO
[38] <Print> → PRINT*, <ExpList>
[39] <Read> → READ*, <VarList>

[40] <ExpList> → <ExprArith> <ExpList-next>
[41] <ExpList-next> → , <ExpList>
[42] → ε
