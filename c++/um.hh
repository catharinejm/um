#ifndef _UM_HH
#define _UM_HH

#include <cstdint>
#include <vector>
#include <stdexcept>
#include <iostream>
#include <iomanip>

typedef uint32_t u32;
typedef uint8_t byte;

typedef std::vector<u32> MemArray;

enum Op {
    MVCOND = 0,
    LOAD = 1,
    STORE = 2,
    ADD = 3,
    MULT = 4,
    DIV = 5,
    NAND = 6,
    HALT = 7,
    ALLOC = 8,
    FREE = 9,
    OUTPUT = 10,
    INPUT = 11,
    LDPROG = 12,
    LDIMM = 13,
};

struct Instruction {
    u32 const op;
    u32 const regA;
    u32 const regB;
    u32 const regC;

    Instruction(u32 ins)
        : op(ins >> 28), regA((ins >> 6) & 0x7), regB((ins >> 3) & 0x7), regC(ins & 0x7) {}

    void print() const {
        using namespace std;
        cout << "OP: " << op << " regA: " << regA << " regB: " << regB << " regC: " << regC << endl;
    }
};

struct Special {
    u32 const op;
    u32 const reg;
    u32 const immed;

    Special(u32 ins)
        : op(ins >> 28), reg((ins >> 25) & 0x7), immed(ins & 0x1FFFFFF) {}

    void print() const {
        using namespace std;
        cout << "OP: " << op << " reg: " << reg << " immed: " << immed << endl;
    }
};

class VM {
    u32 r0;
    u32 r1;
    u32 r2;
    u32 r3;
    u32 r4;
    u32 r5;
    u32 r6;
    u32 r7;

    u32 ip;

    std::vector<MemArray*> memPool;
public:
    VM() : r0(0), r1(0), r2(0), r3(0), r4(0), r5(0), r6(0), r7(0), ip(0), memPool() {} 

    void loadProg(MemArray *prog) {
        if (memPool.empty())
            memPool.push_back(prog);
        else {
            if (memPool[0]) delete memPool[0];
            memPool[0] = prog;
        }
    }

    u32 getReg(u32 rnum) const {
        switch (rnum) {
        case 0:
            return r0;
        case 1:
            return r1;
        case 2:
            return r2;
        case 3:
            return r3;
        case 4:
            return r4;
        case 5:
            return r5;
        case 6:
            return r6;
        case 7:
            return r7;
        default:
            throw std::runtime_error("Invalid register");
            break;
        }
    }

    void setReg(u32 rnum, u32 val) {
        switch (rnum) {
        case 0:
            r0 = val; break;
        case 1:
            r1 = val; break;
        case 2:
            r2 = val; break;
        case 3:
            r3 = val; break;
        case 4:
            r4 = val; break;
        case 5:
            r5 = val; break;
        case 6:
            r6 = val; break;
        case 7:
            r7 = val; break;
        default:
            throw std::runtime_error("Invalid register");
            break;
        }
    }

    MemArray* getArray(u32 idx) const;
    void setArray(u32 idx, MemArray *val);

    u32 newArray(u32 size);
    void dropArray(u32 idx);

    u32 getWord(u32 idx, u32 offest) const;
    void setWord(u32 idx, u32 offset, u32 val);

    void MVCOND(Instruction insxn);
    void LOAD(Instruction insxn);
    void STORE(Instruction insxn);
    void ADD(Instruction insxn);
    void MULT(Instruction insxn);
    void DIV(Instruction insxn);
    void NAND(Instruction insxn);
    void HALT(Instruction insxn);
    void ALLOC(Instruction insxn);
    void FREE(Instruction insxn);
    void OUTPUT(Instruction insxn);
    void INPUT(Instruction insxn);
    void LDPROG(Instruction insxn);
    void LDIMM(Special spcl);

    void run() {
        try {
            for (;;) {
                u32 insWord = getWord(0, ip);
                // std::cout << std::hex;
                // std::cout
                //     << std::setfill('0')
                //     << std::setw(8)
                //     << insWord
                //     << std::endl;
                // std::cout << std::dec;
                Instruction insxn(insWord);
                // insxn.print();
                Special spcl(insWord);
                // spcl.print();
                switch ((Op)insxn.op) {
                case Op::MVCOND: MVCOND(insxn); ip++; break;
                case Op::LOAD: LOAD(insxn); ip++; break;
                case Op::STORE: STORE(insxn); ip++; break;
                case Op::ADD: ADD(insxn); ip++; break;
                case Op::MULT: MULT(insxn); ip++; break;
                case Op::DIV: DIV(insxn); ip++; break;
                case Op::NAND: NAND(insxn); ip++; break;
                case Op::HALT: HALT(insxn); ip++; break;
                case Op::ALLOC: ALLOC(insxn); ip++; break;
                case Op::FREE: FREE(insxn); ip++; break;
                case Op::OUTPUT: OUTPUT(insxn); ip++; break;
                case Op::INPUT: INPUT(insxn); ip++; break;
                case Op::LDPROG: LDPROG(insxn); break; // no ip++
                case Op::LDIMM: LDIMM(spcl); ip++; break;
                default: throw std::runtime_error("illegal opcode"); break;
                }
            }
        } catch (const std::exception &e) {
            std::cerr << "**** ERROR: " << e.what() << std::endl;
            exit(1);
        }
    }

    void dumpState() const;

};

#endif
