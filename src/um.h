#ifndef UM_H
#define UM_H

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef uint32_t u32;

typedef struct _Instruction {
    u32 op : 4;
    u32 _unused : 19;
    u32 rA : 3;
    u32 rB : 3;
    u32 rC : 3;
} Instruction;

typedef struct _Special {
    u32 op : 4;
    u32 rA : 3;
    u32 imm : 25;
} Special;

typedef union _Word {
    u32 word;
    Instruction insxn;
    Special spcl;
} Word;

typedef enum _Op {
    MVCOND = 0,
    LOAD,
    STORE,
    ADD,
    MULT,
    DIV,
    NAND,
    HALT,
    ALLOC,
    FREE,
    OUTPUT,
    INPUT,
    LDPROG,
    LDIMM,
} Op;

typedef struct _Array {
    u32 len;
    Word contents[];
} Array;

typedef struct _MemPool {
    u32 len;
    Array arrays[];
} MemPool;

typedef struct _VM {
    Word r0;
    Word r1;
    Word r2;
    Word r3;
    Word r4;
    Word r5;
    Word r6;
    Word r7;
    MemPool *mem;
} VM;

VM the_vm;

void fail(char *msg) {
    fprintf(stderr, "*** ERROR: %s\n", msg);
    exit(1);
}

static Word ZERO = { .word = 0 };

static inline Word GET(u32 r) {
    switch (r){
    case 0: return the_vm.r0;
    case 1: return the_vm.r1;
    case 2: return the_vm.r2;
    case 3: return the_vm.r3;
    case 4: return the_vm.r4;
    case 5: return the_vm.r5;
    case 6: return the_vm.r6;
    case 7: return the_vm.r7;
    default: fail("ILLEGAL REGISTER ACCESS"); return ZERO;
    }
}


static inline void SET(u32 r, Word val) {
    switch(r) {
    case 0: the_vm.r0 = (val); break;
    case 1: the_vm.r1 = (val); break;
    case 2: the_vm.r2 = (val); break;
    case 3: the_vm.r3 = (val); break;
    case 4: the_vm.r4 = (val); break;
    case 5: the_vm.r5 = (val); break;
    case 6: the_vm.r6 = (val); break;
    case 7: the_vm.r7 = (val); break;
    default: fail("ILLEGAL REGISTER ACCESS"); break;
    }
}

// static inline u32 GETA(Instruction insxn) {
// foo bar = 10;
// asdf;
// }

#define REGA(insxn) ((Word)(insxn)).insxn.rA
#define REGB(insxn) ((Word)(insxn)).insxn.rB
#define REGC(insxn) ((Word)(insxn)).insxn.rC

#define REGSPC(spcl) ((Word)(spcl)).spcl.rA

static inline Word aget(Array *ary, u32 idx) {
    if (ary) {
        if (idx < ary -> len)
            return ary->contents[idx];
        else
            fail("Array index out of bounds!");
    } else
        fail("attempt to get from null array!");

    return ZERO; // unreachable
}

static inline void aset(Array *ary, u32 idx, Word val) {
    if (ary) {
        if (idx < ary -> len)
            ary->contents[idx] = val;
        else
            fail("Array index out of bounds!");
    } else
        fail("attempt to set value in null array!");
}

static inline Array *ARY(u32 idx) {
    if (idx < the_vm.mem->len)
        return &the_vm.mem->arrays[idx];
    else
        fail("MemPool index out of bounds!");
}

#endif
