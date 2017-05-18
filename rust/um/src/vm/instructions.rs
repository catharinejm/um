use std::io;
use std::io::{stdin, stdout, Read, Write};
use std::num::Wrapping;

use vm::*;

pub enum Register {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
}

trait ToRegister : Copy {
    fn to_register(self) -> Result<Register>;
}

impl ToRegister for u32 {
    fn to_register(self) -> Result<Register> {
        match self {
            0 => Ok(Register::R0),
            1 => Ok(Register::R1),
            2 => Ok(Register::R2),
            3 => Ok(Register::R3),
            4 => Ok(Register::R4),
            5 => Ok(Register::R5),
            6 => Ok(Register::R6),
            7 => Ok(Register::R7),
            bad => Err(UMError::InvalidRegister(bad)),
        }
    }
}

pub struct Instruction {
    pub op: Op,
    pub reg_a: Register,
    pub reg_b: Register,
    pub reg_c: Register,
}

impl Instruction {
    pub fn try_build(insxn: u32) -> Result<Option<Instruction>> {
        match (insxn >> 28).to_op()? {
            Op::LDIMM => Ok(None),
            op => Ok(Some(
                Instruction{
                    op,
                    reg_a: ((insxn >> 6) & 0x7).to_register()?,
                    reg_b: ((insxn >> 3) & 0x7).to_register()?,
                    reg_c: (insxn & 0x7).to_register()?,
                })),
        }
    }
}

pub struct Special {
    pub op: Op,
    pub reg: Register,
    pub immed: u32,
}

impl Special {
    pub fn try_build(insxn: u32) -> Result<Option<Special>> {
        match (insxn >> 28).to_op()? {
            op @ Op::LDIMM => Ok(Some(
                Special{
                    op,
                    reg: ((insxn >> 25) & 0x7).to_register()?,
                    immed: insxn & 0x1FFFFFF,
                })),
            _ => Ok(None),
        }
    }
}

impl VM {

    pub fn get_reg(&self, rnum: Register) -> u32 {
        match rnum {
            Register::R0 => self.r0,
            Register::R1 => self.r1,
            Register::R2 => self.r2,
            Register::R3 => self.r3,
            Register::R4 => self.r4,
            Register::R5 => self.r5,
            Register::R6 => self.r6,
            Register::R7 => self.r7,
        }
    }

    pub fn set_reg(&mut self, rnum: Register, val: u32) {
        match rnum {
            Register::R0 => self.r0 = val,
            Register::R1 => self.r1 = val,
            Register::R2 => self.r2 = val,
            Register::R3 => self.r3 = val,
            Register::R4 => self.r4 = val,
            Register::R5 => self.r5 = val,
            Register::R6 => self.r6 = val,
            Register::R7 => self.r7 = val,
        }
    }

    pub fn get_array(&self, index: u32) -> Result<&Vec<u32>> {
        match self.mempool.get(index as usize) {
            None => Err(UMError::UnallocatedArray(index)),
            Some(&None) => Err(UMError::UnallocatedArray(index)),
            Some(&Some(ref ary)) => Ok(ary),
        }
    }

    pub fn get_array_mut(&mut self, index: u32) -> Result<&mut Vec<u32>> {
        match self.mempool.get_mut(index as usize) {
            None => Err(UMError::UnallocatedArray(index)),
            Some(&mut None) => Err(UMError::UnallocatedArray(index)),
            Some(&mut Some(ref mut ary)) => Ok(ary),
        }
    }

    pub fn new_array(&mut self, size: u32) -> u32 {
        let new_ary = vec![0; size as usize];
        match self.mempool.iter().position(|a| a.is_none()) {
            None => { self.mempool.push(Some(new_ary)); (self.mempool.len() as u32) - 1 },
            Some(idx) => { self.mempool[idx] = Some(new_ary); idx as u32 },
        }
    }

    pub fn drop_array(&mut self, index: u32) -> Result<()> {
        if index == 0 {
            return Err(UMError::DeallocZero)
        }
        self.get_array(index)?; // will error if not allocated
        self.mempool[index as usize] = None;
        Ok(())
    }
    
    pub fn get_word(&self, index: u32, offset: u32) -> Result<u32> {
        match self.get_array(index)?.get(offset as usize) {
            None => Err(UMError::OffsetOutOfBounds(offset)),
            Some(x) => Ok(*x),
        }
    }

    pub fn put_word(&mut self, index: u32, offset: u32, word: u32) -> Result<()> {
        let mut ary = self.get_array_mut(index)?;
        if (offset as usize) >= ary.len() {
            return Err(UMError::OffsetOutOfBounds(offset));
        }
        ary[offset as usize] = word;
        Ok(())
    }

    pub fn mvcond(&mut self, insxn: Instruction) -> Result<()> {
        if self.get_reg(insxn.reg_c) != 0 {
            let val = self.get_reg(insxn.reg_b);
            self.set_reg(insxn.reg_a, val);
        }
        Ok(())
    }

    pub fn load(&mut self, insxn: Instruction) -> Result<()> {
        let ary_idx = self.get_reg(insxn.reg_b);
        let offset = self.get_reg(insxn.reg_c);
        let word = self.get_word(ary_idx, offset)?;
        self.set_reg(insxn.reg_a, word);
        Ok(())
    }

    pub fn store(&mut self, insxn: Instruction) -> Result<()> {
        let index = self.get_reg(insxn.reg_a);
        let offset = self.get_reg(insxn.reg_b);
        let value = self.get_reg(insxn.reg_c);
        self.put_word(index, offset, value)
    }

    pub fn add(&mut self, insxn: Instruction) -> Result<()> {
        let value = Wrapping(self.get_reg(insxn.reg_b)) + Wrapping(self.get_reg(insxn.reg_c));
        self.set_reg(insxn.reg_a, value.0);
        Ok(())
    }

    pub fn mult(&mut self, insxn: Instruction) -> Result<()> {
        let value = Wrapping(self.get_reg(insxn.reg_b)) * Wrapping(self.get_reg(insxn.reg_c));
        self.set_reg(insxn.reg_a, value.0);
        Ok(())
    }

    pub fn div(&mut self, insxn: Instruction) -> Result<()> {
        let divisor = self.get_reg(insxn.reg_c);
        if divisor == 0 {
            return Err(UMError::DivideByZero);
        }
        let value = Wrapping(self.get_reg(insxn.reg_b)) / Wrapping(divisor);
        self.set_reg(insxn.reg_a, value.0);
        Ok(())
    }

    pub fn nand(&mut self, insxn: Instruction) -> Result<()> {
        let value = !(self.get_reg(insxn.reg_b) & self.get_reg(insxn.reg_c));
        self.set_reg(insxn.reg_a, value);
        Ok(())
    }

    pub fn halt(&mut self, _: Instruction) -> Result<()> {
        Err(UMError::Halt)
    }

    pub fn alloc(&mut self, insxn: Instruction) -> Result<()> {
        let size = self.get_reg(insxn.reg_c);
        let idx = self.new_array(size);
        self.set_reg(insxn.reg_b, idx);
        Ok(())
    }

    pub fn free(&mut self, insxn: Instruction) -> Result<()> {
        let idx = self.get_reg(insxn.reg_c);
        self.drop_array(idx)
    }

    pub fn output(&mut self, insxn: Instruction) -> Result<()> {
        let wd = self.get_reg(insxn.reg_c);
        if wd > 255 {
            return Err(UMError::InvalidChar(wd))
        }
        let mut out = stdout();
        out.write_all(&[wd as u8])?;
        out.flush()?;
        Ok(())
    }

    pub fn input(&mut self, insxn: Instruction) -> Result<()> {
        let mut buf = [0; 1];
        match stdin().read_exact(&mut buf) {
            Ok(_) => self.set_reg(insxn.reg_c, buf[0] as u32),
            Err(err) => {
                if err.kind() == io::ErrorKind::UnexpectedEof {
                    self.set_reg(insxn.reg_c, !0);
                } else {
                    return Err(UMError::from(err))
                }
            }
        }
        Ok(())
    }

    pub fn ldprog(&mut self, insxn: Instruction) -> Result<()> {
        let ary_idx = self.get_reg(insxn.reg_b);

        let ary = if ary_idx > 0 {
            Some(self.get_array(ary_idx)?.clone())
        } else {
            None
        };

        if ary.is_some() {
            self.mempool[0] = ary
        }

        self.ip = self.get_reg(insxn.reg_c);
        Ok(())
    }

    pub fn ldimm(&mut self, spcl: Special) -> Result<()> {
        self.set_reg(spcl.reg, spcl.immed);
        Ok(())
    }

}
