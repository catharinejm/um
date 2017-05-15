#include "um.hh"
#include <iostream>
#include <fstream>
#include <string>
#include <stdexcept>
#include <stack>
#include <utility>

void LineBuf::push(std::string &&line) {
    if (m_buf.size() == m_cap) {
        m_buf[m_topIdx++] = line;
        if (m_topIdx < m_cap) return;
        m_topIdx = 0;
    } else
        m_buf.push_back(line);
}
void LineBuf::push(std::string &line) {
    push(std::move(line));
}

std::vector<std::string> LineBuf::lines() const {
    std::vector<std::string> inOrderVec;
    for (int i = m_topIdx; i < m_buf.size(); i++)
        inOrderVec.push_back(m_buf[i]);
    for (int i = 0; i < m_topIdx; i++)
        inOrderVec.push_back(m_buf[i]);
    return inOrderVec;
}

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

static inline void dumpWord(std::ofstream &file, u32 word) {
    file << (char)(word >> 24);
    file << (char)(word >> 16 & 0xFF);
    file << (char)(word >> 8 & 0xFF);
    file << (char)(word & 0xFF);
}

void VM::dumpState() const {
    using namespace std;
    ofstream dumpFile("dump.bin", ios_base::binary | ios_base::out);
    dumpWord(dumpFile, r0);
    dumpWord(dumpFile, r1);
    dumpWord(dumpFile, r2);
    dumpWord(dumpFile, r3);
    dumpWord(dumpFile, r4);
    dumpWord(dumpFile, r5);
    dumpWord(dumpFile, r6);
    dumpWord(dumpFile, r7);
    dumpWord(dumpFile, ip);
    dumpWord(dumpFile, (u32)memPool.size());
    for (auto aryP : memPool) {
        if (!aryP) {
            dumpFile << '\0' << '\0' << '\0' << '\0';
        } else {
            dumpWord(dumpFile, (u32)(aryP->size()));
            for (auto word : *aryP)
                dumpWord(dumpFile, word);
        }
    }
    auto ls = lines();
    dumpWord(dumpFile, (u32)ls.size());
    for (auto line : ls)
        dumpFile << line << endl;
    dumpFile << currentLine.str();
}

static inline u32 loadWord(std::ifstream &file) {
    u32 word = 0;
    for (int i = 3; i >= 0; i--) {
        auto b = file.get();
        if (b == -1)
            throw std::runtime_error("premature EOF loading VM dump");
        word |= (((u32)b) & 0xFF) << (8 * i);
    }
    return word;
}

static inline std::string readString(std::ifstream &file) {
    char line[512];
    std::stringstream linebuf;
    do {
        file.getline(line, 512, '\n');
        linebuf << line;
    } while (file.fail());
    return linebuf.str();
}

VM VM::loadState(std::string const &filename) {
    using namespace std;
    cout << "Loading dump file " << filename << "..." << endl;
    ifstream file(filename, ios_base::binary | ios_base::in);
    VM vm;
    vm.r0 = loadWord(file);
    vm.r1 = loadWord(file);
    vm.r2 = loadWord(file);
    vm.r3 = loadWord(file);
    vm.r4 = loadWord(file);
    vm.r5 = loadWord(file);
    vm.r6 = loadWord(file);
    vm.r7 = loadWord(file);
    vm.ip = loadWord(file);
    // cout << "R0: " << vm.r0 << endl
    //      << "R1: " << vm.r1 << endl
    //      << "R2: " << vm.r2 << endl
    //      << "R3: " << vm.r3 << endl
    //      << "R4: " << vm.r4 << endl
    //      << "R5: " << vm.r5 << endl
    //      << "R6: " << vm.r6 << endl
    //      << "R7: " << vm.r7 << endl
    //      << "IP: " << vm.ip << endl;
    u32 memPoolSize = loadWord(file);
    for (u32 i = 0; i < memPoolSize; i++) {
        u32 arySize = loadWord(file);
        // cout << "Loading MemArray of size " << arySize << "..." << endl;
        if (!arySize)
            vm.memPool.push_back(nullptr);
        else {
            auto ary = new MemArray();
            for (u32 i = 0; i < arySize; i++) {
                ary->push_back(loadWord(file));
            }
            vm.memPool.push_back(ary);
        }
    }
    const int numLines = (int)loadWord(file);
    for (u32 i = 0; i < numLines; i++) {
        vm.m_lineBuf.push(readString(file));
    }
    
    vm.currentLine.str(readString(file));
    return vm;
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

    if (c == '\n') {
        m_lineBuf.push(currentLine.str());
        currentLine.str("");
    } else
        currentLine << (char)c;

    std::cout << (char)c;
}

void VM::INPUT(Instruction insxn) {
    int c;
    do {
        c = std::cin.get();
        if (c == 24) { // C-x
            if (std::cin.get() == 'd') {
                std::cout << "dumping" << std::endl;
                dumpState();
                if (std::cin.peek() == '\n')
                    std::cin.get();

            }
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

void VM::loadProgramFile(std::string &filename) {
    using namespace std;
    ifstream infile(filename, ios_base::binary | ios_base::in);
    auto *buffer = new vector<char>(std::istreambuf_iterator<char>(infile), std::istreambuf_iterator<char>());
    if (! buffer) {
        cerr << "Failed to allocate buffer" << endl;
        exit(1);
    }
        
    if (buffer->size() % 4 != 0) {
        cerr << "program is not word aligned" << endl;
        exit(1);
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

    loadProg(prog);
}

int main(int argc, char *argv[]) {
    using namespace std;

    cout.setf(ios_base::unitbuf);

    if (argc < 2) {
        cerr << "Usage: UM ( <um program> | --load <vmdump> )" << endl;
        return 1;
    }

    stack<string> args;
    for (int i = argc-1; i > 0; i--)
        args.push(string(argv[i]));

    bool loadDump = false;
    if (args.top() == "--load") {
        loadDump = true;
        args.pop();
        if (args.empty()) {
            cerr << "'--load' requires an argument" << endl;
            exit(1);
        }
    }

    if (loadDump) {
        VM vm = VM::loadState(args.top());
        for (auto line : vm.lines())
            cout << line << endl;
        cout << vm.curLine();
        vm.run();
    } else {
        VM vm;
        vm.loadProgramFile(args.top());
        vm.run();
    }

    return 0;
}
