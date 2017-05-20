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

    let mut vm = VM::new();

    let result = if args[1] == "--load" {
        match args.get(2) {
            None => {
                println!("option --load requires an argument");
                exit(1);
            }
            Some(f) => vm.load_dump(f).and_then(|_| vm.run()),
        }
    } else {
        vm.load_program_file(&args[1]).and_then(|_| vm.run())
    };

    match result {
        Err(UMError::Halt) => (),
        Err(err) => {
            println!("**** ERROR: {}", err);
            exit(1)
        }
        Ok(_) => (),
    }

}
