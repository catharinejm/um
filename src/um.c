#include <string.h>

#include "um.h"

u32 vm_alloc(u32 size) {
    u32 i = 1;
    for (; i < the_vm.mem.len; i++) {
        if (!the_vm.mem.arrays[i]) break;
    }
    if (i < the_vm.mem.len) {
        the_vm.mem.arrays[i] = vm_new_ary(size);
        return i;
    } else {
        return vm_append_ary(size);
    }
}

void vm_free(u32 idx) {
    if (idx > 0) {
        if (idx < the_vm.mem.len) {
            if (the_vm.mem.arrays[idx]) {
                free(the_vm.mem.arrays+idx);
                the_vm.mem.arrays[idx] = NULL;
            } else
                fail("Attempt to free already freed array");
        } else
            fail("Attempt to free unallocated array");
    } else
        fail("Attempt to free array 0");
}

u32 vm_append_ary(u32 size) {
    if (the_vm.mem.len >= the_vm.mem.cap)
        vm_grow_mem();
    u32 i = the_vm.mem.len++;
    the_vm.mem.arrays[i] = vm_new_ary(size);
    return i;
}

Array *vm_new_ary(u32 size) {
    Array *ary = malloc(sizeof(Array) + size*sizeof(Word));
    if (!ary)
        fail("Failed to allocate array");
    ary->len = size;
    memset(ary->contents, 0, size*sizeof(Word));
    return ary;
}

void vm_ary_copy(u32 dest_idx, u32 src_idx) {
    vm_free(dest_idx);
    Array* src = ARY(src_idx);
    Array* ary = vm_new_ary(src->len);
    memcpy(ary->contents, src->contents, ary->len);
}

void vm_grow_mem() {
    MemPool pool = {
        .len = the_vm.mem.len,
        .cap = the_vm.mem.cap * 2,
        .arrays = NULL,
    };
    size_t size = sizeof(Array*)*pool.cap;
    Array** arys = (Array**)realloc(the_vm.mem.arrays, size);
    if (!arys)
        fail("Failed to resize memory pool");
    memset(arys, 0, size);
    memcpy(arys, the_vm.mem.arrays, pool.len);
    pool.arrays = arys;
    the_vm.mem = pool;
}

void vm_op_mvcond(Instruction insxn) {
    if (GETC(insxn).word) {
        SETA(insxn, GETB(insxn));
    }
}

void vm_op_load(Instruction insxn) {
    Array *src = ARY(GETB(insxn).word);
    u32 idx = GETC(insxn).word;
    SETA(insxn, aget(src, idx));
}

void vm_op_store(Instruction insxn) {
    aset(ARY(GETA(insxn).word), GETB(insxn).word, GETC(insxn));
}

void vm_op_add(Instruction insxn) {
    SETA(insxn, GETB(insxn).word + GETC(insxn).word);
}

void vm_op_mult(Instruction insxn) {
    SETA(insxn, GETB(insxn).word * GETC(insxn).word);
}

void vm_op_div(Instruction insxn) {
    SETA(insxn, GETB(insxn).word / GETC(insxn).word);
}

void vm_op_nand(Instruction insxn) {
    SETA(insxn, ~(GETB(insxn).word & GETC(insxn).word));
}

void vm_op_halt(Instruction insxn) {
    printf("Goodbye...\n");
    exit(0);
}

void vm_op_alloc(Instruction insxn) {
    u32 new_idx = vm_alloc(GETC(insxn).word);
    SETB(insxn, new_idx);
}

void vm_op_free(Instruction insxn) {
    vm_free(GETC(insxn).word);
}

void vm_op_output(Instruction insxn) {
    u32 c = GETC(insxn).word;
    if (c > 255)
        fail("Illegal character value");
    printf("%c", (char)c);
}

void vm_op_input(Instruction insxn) {
    int c = getchar();
    SETC(insxn, (u32)c);
}

void vm_op_ldprog(Instruction insxn) {
    u32 idx = GETB(insxn).word;
    if (idx == 0) return;
    vm_ary_copy(0, idx);
    the_vm.ip = GETC(insxn).word;
}

void vm_op_ldimm(Special spcl) {
    SET(GETSPC(spcl).word, (Word)(u32)spcl.imm);
}

void decode_and_run() {
    Word word = aget(ARY(0), the_vm.ip);
    switch ((Op)word.base.op) {
    case MVCOND:
        vm_op_mvcond(word.insxn); break;
    case LOAD:
        vm_op_load(word.insxn); break;
    case STORE:
        vm_op_store(word.insxn); break;
    case ADD:
        vm_op_add(word.insxn); break;
    case MULT:
        vm_op_mult(word.insxn); break;
    case DIV:
        vm_op_div(word.insxn); break;
    case NAND:
        vm_op_nand(word.insxn); break;
    case HALT:
        vm_op_halt(word.insxn); break;
    case ALLOC:
        vm_op_alloc(word.insxn); break;
    case FREE:
        vm_op_free(word.insxn); break;
    case OUTPUT:
        vm_op_output(word.insxn); break;
    case INPUT:
        vm_op_input(word.insxn); break;
    case LDPROG:
        vm_op_ldprog(word.insxn); break;
    case LDIMM:
        vm_op_ldimm(word.spcl); break;
    default:
        fail("illegal instruction");
    }
}

int main(int argc, char *argv[]) {
    setbuf(stdout, NULL); // disable stdout buffering
    printf("Ummm...\n");

    return 0;
}
