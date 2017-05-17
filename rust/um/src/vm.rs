use std::convert::{From};
use std::error;
use std::fmt;
use std::fs::{File};
use std::io;
use std::io::{Read};
use std::path::{Path};
use std::result;

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
    fn to_word(self) -> u32 {
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
    IO(io::Error),
}

impl From<io::Error> for UMError {
    fn from(err: io::Error) -> UMError {
        UMError::IO(err)
    }
}

impl fmt::Display for UMError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            UMError::BadOpcode(opcode) => write!(f, "Bad Opcode: {}", opcode),
            UMError::InvalidFile(msg) => write!(f, "Invalid file: {}", msg),
            UMError::IO(ref err) => write!(f, "{}", err),
        }
    }
}

impl error::Error for UMError {
    fn description(&self) -> &str {
        match *self {
            UMError::BadOpcode(_) => "Bad opcode",
            UMError::InvalidFile(_) => "Invalid file",
            UMError::IO(ref err) => err.description(),
        }
    }

    fn cause(&self) -> Option<&error::Error> {
        match *self {
            UMError::BadOpcode(_) => None,
            UMError::InvalidFile(_) => None,
            UMError::IO(ref err) => Some(err),
        }
    }
}

type Result<T> = result::Result<T, UMError>;

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

    pub mempool: Vec<Vec<u32>>,
}

fn build_words(acc: Result<(Vec<u32>, Vec<u8>)>, byte: io::Result<u8>) -> Result<(Vec<u32>, Vec<u8>)> {
    match acc? {
        (mut words, mut cur_word) => {
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
            Ok((words, cur_word))
        }
    }
}

impl VM {
    pub fn load_program_file<P : AsRef<Path>>(&mut self, path: P) -> Result<()> {
        let bytes = File::open(path)?.bytes();
        let words = match bytes.fold(Ok((vec![], vec![])), build_words)? {
            (wds, cur_words) =>
                if !cur_words.is_empty() {
                    return Err(UMError::InvalidFile("not 4-byte word aligned"));
                } else {
                    wds
                }
        };
        self.mempool.push(words);
        Ok(())
    }

    pub fn run() -> Result<()> {
        Ok(())
    }
}
