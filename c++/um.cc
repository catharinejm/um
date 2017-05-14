#include "um.hh"
#include <iostream>
#include <fstream>
#include <string>
#include <stdexcept>

MemArray *VM::getArray(u32 idx) const {
    if (idx >= memPool.size())
        throw std::runtime_error("index outside mempool bounds");
    if (!memPool[idx])
        throw std::runtime_error("accessing unallocated array");
    return memPool[idx];
}

void VM::setArray(u32 idx, MemArray *val) {
    if (idx >= memPool.size())
        throw std::runtime_error("index outside mempool bounds");
    if (memPool[idx])
        delete memPool[idx];
    memPool[idx] = val;
}

u32 VM::newArray(u32 size) {
    int i = 1;
    for (; i < memPool.size(); i++) {
        if (!memPool[i]) {
            memPool[i] = new MemArray(size, 0);
            return i;
        }
    }
    memPool.push_back(new MemArray(size, 0));
    return i;
}

void VM::dropArray(u32 idx) {
    if (idx >= memPool.size() || !memPool[idx])
        throw std::runtime_error("attempt to drop unallocated array");
    delete memPool[idx];
    memPool[idx] = nullptr;
}

u32 VM::getWord(u32 idx, u32 offset) const {
    auto ary = getArray(idx);
    if (offset >= ary->size())
        throw std::runtime_error("index outside of array bounds");
    return ary->at(offset);
}

void VM::setWord(u32 idx, u32 offset, u32 val) {
    auto ary = getArray(idx);
    if (offset >= ary->size())
        throw std::runtime_error("index outside of array bounds");
    (*ary)[offset] = val;
}

void VM::dumpState() const {
    // dump VM to file
}

void VM::MVCOND(Instruction insxn) {
    if (getReg(insxn.regC))
        setReg(insxn.regA, getReg(insxn.regB));
}

void VM::LOAD(Instruction insxn) {
    setReg(insxn.regA, getWord(getReg(insxn.regB), getReg(insxn.regC)));
}

void VM::STORE(Instruction insxn) {
    setWord(getReg(insxn.regA), getReg(insxn.regB), getReg(insxn.regC));
}

void VM::ADD(Instruction insxn) {
    setReg(insxn.regA, getReg(insxn.regB) + getReg(insxn.regC));
}


void VM::MULT(Instruction insxn) {
    setReg(insxn.regA, getReg(insxn.regB) * getReg(insxn.regC));
}

void VM::DIV(Instruction insxn) {
    setReg(insxn.regA, getReg(insxn.regB) / getReg(insxn.regC));
}

void VM::NAND(Instruction insxn) {
    setReg(insxn.regA, ~(getReg(insxn.regB) & getReg(insxn.regC)));
}

void VM::HALT(Instruction insxn) {
    exit(0);
}

void VM::ALLOC(Instruction insxn) {
    auto idx = newArray(getReg(insxn.regC));
    setReg(insxn.regB, idx);
}

void VM::FREE(Instruction insxn) {
    dropArray(getReg(insxn.regC));
}

void VM::OUTPUT(Instruction insxn) {
    u32 c = getReg(insxn.regC);
    if (c > 255)
        throw std::runtime_error("invalid character");
    std::cout << (char)c;
}

void VM::INPUT(Instruction insxn) {
    char c;
    do {
        c = std::cin.get();
        
        if (c == 24) { // C-x
            if (stc::cin.get() == 'd')
                dumpState();
        }
    } while (c == 24);

    setReg(insxn.regC, (u32)c);
}
void VM::LDPROG(Instruction insxn) {
    auto idx = getReg(insxn.regB);
    if (idx > 0) {
        auto ary = getArray(idx);
        auto newAry = new MemArray(*ary);
        if (memPool[0])
            delete memPool[0];
        memPool[0] = newAry;
    }
    ip = getReg(insxn.regC);
}

void VM::LDIMM(Special spcl) {
    setReg(spcl.reg, spcl.immed);
}

int main(int argc, char *argv[]) {
    using namespace std;

    cout.setf(ios_base::unitbuf);

    VM vm = VM();

    if (argc != 2) {
        cerr << "Usage: UM <file>" << endl;
        return 1;
    }

    ifstream infile(argv[1], ios_base::binary | ios_base::in);
    auto *buffer = new vector<char>(std::istreambuf_iterator<char>(infile), std::istreambuf_iterator<char>());
    if (! buffer) {
        cerr << "Failed to allocate buffer" << endl;
        return 1;
    }
        
    if (buffer->size() % 4 != 0) {
        cerr << "program is not word aligned" << endl;
        return 1;
    }

    MemArray *prog = new MemArray();

    for (int i = 0; i < buffer->size(); i+=4) {
        u32 b0 = (u32)buffer->at(i) & 0xFF;
        u32 b1 = (u32)buffer->at(i+1) & 0xFF;
        u32 b2 = (u32)buffer->at(i+2) & 0xFF;
        u32 b3 = (u32)buffer->at(i+3) & 0xFF;
        prog->push_back((b0 << 24) | (b1 << 16) | (b2 << 8) | b3);
    }

    delete buffer;

    vm.loadProg(prog);
    vm.run();

    return 0;
}
