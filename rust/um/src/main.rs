extern crate um;

use um::vm::*;

use std::env;
use std::process::exit;

fn main() {
    let mut args: Vec<String> = vec![];

    for arg in env::args() {
        args.push(arg);
    }

    if args.len() < 2 {
        println!("usage: UM ( <um file> | --load <dump file> )");
        exit(1);
    }

    let load_dump = args[1] == "--load";

    let filepath = if load_dump {
        match args.get(2) {
            None => {
                println!("option --load requires an argument");
                exit(1);
            }
            Some(f) => f
        }
    } else {
        &args[1]
    };
    
    let mut vm = VM {
        r0: 0,
        r1: 0,
        r2: 0,
        r3: 0,
        r4: 0,
        r5: 0,
        r6: 0,
        r7: 0,

        ip: 0,
        
        mempool: vec![],
    };

    match vm.load_program_file(filepath) {
        Ok(_) => (),
        Err(err) => {
            println!("Error loading file: {}", err);
            exit(1);
        }
    }

    match vm.run() {
        Err(UMError::Halt) => (),
        Err(err) => {
            println!("**** ERROR: {}", err);
            exit(1)
        },
        Ok(_) => (),
    }

}
