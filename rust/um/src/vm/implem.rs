use std::convert::{From};
use std::error;
use std::fmt;
use std::fs::{File};
use std::io;
use std::io::{Read};
use std::path::{Path};
use std::result;

use vm::instructions::{Instruction, Special};

#[derive(Debug, PartialEq, Eq)]
pub enum Op {
    MVCOND,
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
}

impl Op {
    pub fn to_word(self) -> u32 {
        match self {
            Op::MVCOND => 0,
            Op::LOAD => 1,
            Op::STORE => 2,
            Op::ADD => 3,
            Op::MULT => 4,
            Op::DIV => 5,
            Op::NAND => 6,
            Op::HALT => 7,
            Op::ALLOC => 8,
            Op::FREE => 9,
            Op::OUTPUT => 10,
            Op::INPUT => 11,
            Op::LDPROG => 12,
            Op::LDIMM => 13,
        }
    }
}

#[derive(Debug)]
pub enum UMError {
    BadOpcode(u32),
    InvalidFile(&'static str),
    InvalidRegister(u32),
    UnallocatedArray(u32),
    OffsetOutOfBounds(u32),
    DeallocZero,
    InvalidChar(u32),
    InvalidInstruction(u32),
    DivideByZero,
    IO(io::Error),
    Halt,
}

impl From<io::Error> for UMError {
    fn from(err: io::Error) -> UMError {
        UMError::IO(err)
    }
}

impl fmt::Display for UMError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            UMError::BadOpcode(opcode) => write!(f, "Bad Opcode: {:x}", opcode),
            UMError::InvalidFile(msg) => write!(f, "Invalid file: {}", msg),
            UMError::InvalidRegister(rnum) => write!(f, "Invalid register: {}", rnum),
            UMError::UnallocatedArray(idx) => write!(f, "Attempt to access unallocated array index: {}", idx),
            UMError::OffsetOutOfBounds(off) => write!(f, "Array offset out of bounds: {}", off),
            UMError::DeallocZero => write!(f, "Attempt to de-allocate array 0"),
            UMError::InvalidChar(chr) => write!(f, "Invalid character: {}", chr),
            UMError::InvalidInstruction(insxn) => write!(f, "Invalid instruction: {:x}", insxn),
            UMError::DivideByZero => write!(f, "Divide by zero"),
            UMError::IO(ref err) => write!(f, "{}", err),
            UMError::Halt => write!(f, "Halting VM"),
        }
    }
}

impl error::Error for UMError {
    fn description(&self) -> &str {
        match *self {
            UMError::BadOpcode(_) => "Bad opcode",
            UMError::InvalidFile(_) => "Invalid file",
            UMError::InvalidRegister(_) => "Invalid register",
            UMError::UnallocatedArray(_) => "Attempt to access unallocated array index",
            UMError::OffsetOutOfBounds(_) => "Array offset out of bounds",
            UMError::DeallocZero => "Attempt to de-allocate array 0",
            UMError::InvalidChar(_) => "Invalid character",
            UMError::InvalidInstruction(_) => "Invalid instruction",
            UMError::DivideByZero => "Divide by zero",
            UMError::IO(ref err) => err.description(),
            UMError::Halt => "Halting VM" 
        }
    }

    fn cause(&self) -> Option<&error::Error> {
        match *self {
            UMError::IO(ref err) => Some(err),
            _ => None
        }
    }
}

pub type Result<T> = result::Result<T, UMError>;

pub trait ToOp : Copy {
    fn to_op(self) -> Result<Op>;
}

impl ToOp for u32 {
    fn to_op(self) -> Result<Op> {
        match self {
            0 => Ok(Op::MVCOND),
            1 => Ok(Op::LOAD),
            2 => Ok(Op::STORE),
            3 => Ok(Op::ADD),
            4 => Ok(Op::MULT),
            5 => Ok(Op::DIV),
            6 => Ok(Op::NAND),
            7 => Ok(Op::HALT),
            8 => Ok(Op::ALLOC),
            9 => Ok(Op::FREE),
            10 => Ok(Op::OUTPUT),
            11 => Ok(Op::INPUT),
            12 => Ok(Op::LDPROG),
            13 => Ok(Op::LDIMM),
            op => Err(UMError::BadOpcode(op)),
        }
    }
}

pub struct VM {
    pub r0: u32,
    pub r1: u32,
    pub r2: u32,
    pub r3: u32,
    pub r4: u32,
    pub r5: u32,
    pub r6: u32,
    pub r7: u32,

    pub ip: u32,

    pub mempool: Vec<Option<Vec<u32>>>,
}

struct WordAcc {
    words: Vec<u32>,
    cur_word: Vec<u8>,
}

impl WordAcc {
    fn empty() -> WordAcc {
        WordAcc { words: vec![], cur_word: vec![] }
    }
}

fn build_words(acc: Result<WordAcc>, byte: io::Result<u8>) -> Result<WordAcc> {
    match acc? {
        WordAcc { mut words, mut cur_word } => {
            if cur_word.len() == 3 {
                let mut wd: u32 = 0;
                for (b, i) in cur_word.iter().zip((0..)) {
                    wd |= (*b as u32) << ((3 - i) * 8)
                }
                wd |= byte? as u32;
                words.push(wd);
                cur_word.clear();
            } else {
                cur_word.push(byte.unwrap());
            }
            Ok(WordAcc{words, cur_word})
        }
    }
}

impl VM {
    pub fn load_program_file<P : AsRef<Path>>(&mut self, path: P) -> Result<()> {
        let bytes = File::open(path)?.bytes();
        let words = match bytes.fold(Ok(WordAcc::empty()), build_words)? {
            WordAcc { words: wds, cur_word } =>
                if !cur_word.is_empty() {
                    return Err(UMError::InvalidFile("not 4-byte word aligned"));
                } else {
                    wds
                }
        };
        self.mempool.push(Some(words));
        Ok(())
    }

    pub fn run(&mut self) -> Result<()> {
        loop {
            let insxn_word = self.get_word(0, self.ip)?;
            match Instruction::try_build(insxn_word)? {
                Some(insxn) => match insxn.op {
                    Op::MVCOND => self.mvcond(insxn)?,
                    Op::LOAD => self.load(insxn)?,
                    Op::STORE => self.store(insxn)?,
                    Op::ADD => self.add(insxn)?,
                    Op::MULT => self.mult(insxn)?,
                    Op::DIV => self.div(insxn)?,
                    Op::NAND => self.nand(insxn)?,
                    Op::HALT => self.halt(insxn)?,
                    Op::ALLOC => self.alloc(insxn)?,
                    Op::FREE => self.free(insxn)?,
                    Op::OUTPUT => self.output(insxn)?,
                    Op::INPUT => self.input(insxn)?,
                    Op::LDPROG => self.ldprog(insxn)?,
                    Op::LDIMM => return Err(UMError::InvalidInstruction(insxn_word)),
                },
                None => {
                    match Special::try_build(insxn_word)? {
                        Some(spcl) => self.ldimm(spcl)?,
                        None => return Err(UMError::InvalidInstruction(insxn_word)),
                    }
                },
            }
            if (insxn_word >> 28).to_op()? != Op::LDPROG {
                self.ip += 1;
            }
        }
    }
}
