#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <unistd.h>
#include <fcntl.h>
#include <assert.h>
#include <signal.h>

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
    assert(the_vm.mem.len <= the_vm.mem.cap);
    if (the_vm.mem.len == the_vm.mem.cap)
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
    memset(arys+the_vm.mem.cap, 0, sizeof(Array*)*(pool.cap-the_vm.mem.cap));
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
    u32 div = GETC(insxn).word;
    if (div == 0)
        fail("divide by zero!");
    SETA(insxn, GETB(insxn).word / div);
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
    fprintf(stderr, "%c", (char)c);
}

void vm_op_input(Instruction insxn) {
    int c = getchar();
    SETC(insxn, (u32)c);
}

void vm_op_ldprog(Instruction insxn) {
    u32 idx = GETB(insxn).word;
    if (idx != 0)
        vm_ary_copy(0, idx);
    the_vm.ip = GETC(insxn).word;
}

void vm_op_ldimm(Special spcl) {
    SETSPC(spcl, (u32)spcl.imm);
}

void decode_and_run() {
    for (;;) {
        u32 word = aget(ARY(0), the_vm.ip).word;
        Word w = (Word)((u32)0 | ((word & 0xFF) << 24) | (((word >> 8) & 0xFF) << 16) | (((word >> 16) & 0xFF) << 8) | (word >> 24));
        fprintf(stderr, "%08x\n", w.word);
        vm_dump();
        switch ((Op)w.base.op) {
        case MVCOND:
            vm_op_mvcond(w.insxn); break;
        case LOAD:
            vm_op_load(w.insxn); break;
        case STORE:
            vm_op_store(w.insxn); break;
        case ADD:
            vm_op_add(w.insxn); break;
        case MULT:
            vm_op_mult(w.insxn); break;
        case DIV:
            vm_op_div(w.insxn); break;
        case NAND:
            vm_op_nand(w.insxn); break;
        case HALT:
            vm_op_halt(w.insxn); break;
        case ALLOC:
            vm_op_alloc(w.insxn); break;
        case FREE:
            vm_op_free(w.insxn); break;
        case OUTPUT:
            vm_op_output(w.insxn); break;
        case INPUT:
            vm_op_input(w.insxn); break;
        case LDPROG:
            vm_op_ldprog(w.insxn); break;
        case LDIMM:
            vm_op_ldimm(w.spcl); break;
        default:
            fail("illegal instruction");
        }
        if ((Op)w.base.op != LDPROG)
            the_vm.ip++;
    }
}

void init_vm() {
    the_vm.r0 = ZERO;
    the_vm.r1 = ZERO;
    the_vm.r2 = ZERO;
    the_vm.r3 = ZERO;
    the_vm.r4 = ZERO;
    the_vm.r5 = ZERO;
    the_vm.r6 = ZERO;
    the_vm.r7 = ZERO;
    the_vm.ip = 0;
    the_vm.mem.len = 1;
    the_vm.mem.cap = 16;
    the_vm.mem.arrays = malloc(16*sizeof(Array*));
    if (!the_vm.mem.arrays)
        fail("Failed to initialize mempool");
}

void handler(int signum) {
    vm_dump();
}

int main(int argc, char *argv[]) {
    setbuf(stdout, NULL); // disable stdout buffering

    if (argc != 2) {
        fprintf(stderr, "Usage: %s <file>\n", argv[0]);
        return 1;
    }
    
    init_vm();
    int fd = open(argv[1], O_RDONLY);
    if (fd < 0)
        fail("failed to open file");
    struct stat st;
    if (stat(argv[1], &st))
        fail("failed ot stat file");
    void *fp = mmap(NULL, st.st_size, PROT_READ | PROT_WRITE, MAP_PRIVATE, fd, 0);
    if ((intptr_t)fp < 0)
        fail("failed to mmap file");
    Array *zero_ary = vm_new_ary((u32)st.st_size);
    memcpy(zero_ary->contents, fp, st.st_size);
    munmap(fp, st.st_size);
    the_vm.mem.arrays[0] = zero_ary;
    signal(SIGUSR1, handler);
    decode_and_run();

    return 0;
}
