#include <stdio.h>
#include <stdint.h>

#include "um.h"

void vm_op_mvcond(Instruction insxn) {
    if (GET(REGC(insxn)).word) {
        SET(REGA(insxn), GET(REGB(insxn)));
    }
}

void vm_op_load(Instruction insxn) {
    Array *src = ARY(GET(REGB(insxn)).word);
    u32 idx = GET(REGC(insxn)).word;
    SET(REGA(insxn), aget(src, idx));
}

void vm_op_store(Instruction insxn) {
    aset(ARY(GET(REGA(insxn)).word), GET(REGB(insxn)).word, GET(REGC(insxn)));
}

int main(int argc, char *argv[]) {
    printf("Ummm...\n");

    return 0;
}
