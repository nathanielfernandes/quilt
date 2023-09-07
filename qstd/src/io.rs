use std::io::Write;

use common::error::Error;
use interpreter::{builtins::*, generic_builtins, value::Value, vm::*};

generic_builtins! {
    [export=io]
    [vm_options=_options]

    fn @print(to_print: rest) {
        for arg in to_print {
            print!("{} ", arg);
        }
        println!();
        Value::None
    }

    fn @debug(to_print: rest) {
        // orange
        print!("\x1b[38;5;208mDEBUG: ");

        for arg in to_print {
            print!("{} ", arg);
        }
        // reset
        println!("\x1b[0m");
        Value::None
    }

    fn @input(prompt: str) {
        print!("{}", prompt);
        if let Ok(_) = std::io::stdout().flush() {
            let mut input = String::new();
            if let Ok(_) = std::io::stdin().read_line(&mut input) {
                input.trim().to_string().into()
            } else {
                Err(error("could not read input".to_string()))?
            }
        } else {
            Err(error("could not flush stdout".to_string()))?
        }
    }
}