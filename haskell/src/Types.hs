module Types where



data Op = MVCOND
        | LOAD
        | STORE
        | ADD
        | MULT
        | DIV
        | NAND
        | HALT
        | ALLOC
        | FREE
        | OUTPUT
        | INPUT
        | LDPROG
        | LDIMM
