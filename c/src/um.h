#ifndef UM_H
#define UM_H

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef uint32_t u32;

typedef struct _InstructionBase {
    u32 op : 4;
} InstructionBase;

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
    InstructionBase base;
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
    Word contents[0];
} Array;

typedef struct _MemPool {
    u32 len;
    u32 cap;
    Array **arrays;
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
    u32 ip;
    MemPool mem;
} VM;

VM the_vm;

void vm_dump(void);

void fail(char *msg) {
    fprintf(stderr, "*** ERROR: %s\n", msg);
    vm_dump();
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
    case 0: the_vm.r0 = val; break;
    case 1: the_vm.r1 = val; break;
    case 2: the_vm.r2 = val; break;
    case 3: the_vm.r3 = val; break;
    case 4: the_vm.r4 = val; break;
    case 5: the_vm.r5 = val; break;
    case 6: the_vm.r6 = val; break;
    case 7: the_vm.r7 = val; break;
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

#define GETA(insxn) GET(REGA(insxn))
#define GETB(insxn) GET(REGB(insxn))
#define GETC(insxn) GET(REGC(insxn))

#define SETA(insxn, val) SET(REGA(insxn), (Word)(val))
#define SETB(insxn, val) SET(REGB(insxn), (Word)(val))
#define SETC(insxn, val) SET(REGC(insxn), (Word)(val))

#define REGSPC(spcl) ((Word)(spcl)).spcl.rA
#define GETSPC(spcl) GET(REGSPC(spcl))
#define SETSPC(spcl, val) SET(REGSPC(spcl), (Word)(val))

static inline Word aget(Array *ary, u32 idx) {
    if (ary) {
        if (idx < ary->len)
            return ary->contents[idx];
        else
            fail("Array index out of bounds!");
    } else
        fail("attempt to get from null array!");

    return ZERO; // unreachable
}

static inline void aset(Array *ary, u32 idx, Word val) {
    if (ary) {
        if (idx < ary->len)
            ary->contents[idx] = val;
        else
            fail("Array index out of bounds!");
    } else
        fail("attempt to set value in null array!");
}

static inline Array *ARY(u32 idx) {
    if (idx < the_vm.mem.len)
        return the_vm.mem.arrays[idx];
    else
        fail("MemPool index out of bounds!");
}

u32 vm_alloc(u32 size);
u32 vm_append_ary(u32 size);
Array *vm_new_ary(u32 size);
void vm_ary_copy(u32 dest_idx, u32 src_idx);
void vm_grow_mem();
void vm_free(u32 idx);
void decode_and_run(void);

int op_str(Op op, char * const buf, int bsize) {
    char * s;
    switch (op) {
    case MVCOND: s = "MVCOND"; break;
    case LOAD: s = "LOAD"; break;
    case STORE: s = "STORE"; break;
    case ADD: s = "ADD"; break;
    case MULT: s = "MULT"; break;
    case DIV: s = "DIV"; break;
    case NAND: s = "NAND"; break;
    case HALT: s = "HALT"; break;
    case ALLOC: s = "ALLOC"; break;
    case FREE: s = "FREE"; break;
    case OUTPUT: s = "OUTPUT"; break;
    case INPUT: s = "INPUT"; break;
    case LDPROG: s = "LDPROG"; break;
    case LDIMM: s = "LDIMM"; break;
    default: s = "<INVALID OP>"; break;
    }
    return snprintf(buf, bsize, "%s (%u)", s, (u32)op);
}

char * const print_insxn(Word wd, char * const buf, int bsize) {
    memset(buf, 0, bsize);
    
    int len = snprintf(buf, bsize, "%x - OP: ", wd.word);
    len += op_str((Op)wd.base.op, buf+len, bsize-len);
    if ((Op)wd.base.op == LDIMM)
        snprintf(buf+len, bsize-len, " - RA: %u - IMM: %u", wd.spcl.rA, wd.spcl.imm);
    else
        snprintf(buf+len, 256-len, " - RA: %u - RB: %u - RC: %u", wd.insxn.rA, wd.insxn.rB, wd.insxn.rC);
    buf[bsize-1] = '\0';
    return buf;        
}

void vm_dump() {
    fprintf(stderr, "R0: %u\n", the_vm.r0.word);
    fprintf(stderr, "R1: %u\n", the_vm.r1.word);
    fprintf(stderr, "R2: %u\n", the_vm.r2.word);
    fprintf(stderr, "R3: %u\n", the_vm.r3.word);
    fprintf(stderr, "R4: %u\n", the_vm.r4.word);
    fprintf(stderr, "R5: %u\n", the_vm.r5.word);
    fprintf(stderr, "R6: %u\n", the_vm.r6.word);
    fprintf(stderr, "R7: %u\n", the_vm.r7.word);
    fprintf(stderr, "IP: %u\n", the_vm.ip);
    char buf[256];
    print_insxn(aget(ARY(0), the_vm.ip), buf, 256);
    fprintf(stderr, "Current instruction: %s\n", buf);
    fprintf(stderr, "Arrays: %u (cap: %u)\n", the_vm.mem.len, the_vm.mem.cap);
}


#endif
