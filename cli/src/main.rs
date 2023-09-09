use clap::Parser;
use quilt::prelude::*;
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(name = "quilt")]
#[command(author = "Nathaniel F. <nathaniel.s.fernandes@gmail.com>")]
#[command(version = "1.0")]
#[command(about = "quilt runtime", long_about = None)]
struct Args {
    #[clap(name = "main-file", help = "Main file to run")]
    main_file: PathBuf,

    #[clap(
        long,
        default_value = "false",
        help = "Prints the disassembly of the script before running it"
    )]
    disassemble: bool,

    #[clap(
        short,
        long,
        default_value = "false",
        help = "Prints the time it took to run the script"
    )]
    timeit: bool,

    #[clap(
        short,
        long,
        default_value = "false",
        help = "Prints various debug information"
    )]
    debug: bool,

    #[clap(
        short,
        long,
        default_value = "false",
        help = "Prints the return value of the script (if any)"
    )]
    result: bool,

    #[clap(long, help = "Maximum runtime in milliseconds")]
    max_runtime: Option<u64>,

    #[clap(long, help = "Maximum stack size")]
    stack_size: Option<usize>,

    #[clap(long, help = "Maximum call stack size")]
    call_stack_size: Option<usize>,

    #[clap(long, help = "Maximum string size in bytes")]
    string_max_size: Option<usize>,

    #[clap(long, help = "Maximum array size in bytes")]
    array_max_size: Option<usize>,
}

fn main() {
    let mut debug = std::time::Instant::now();
    let args = Args::parse();

    let name = args.main_file.display().to_string();
    let src = match std::fs::read_to_string(&args.main_file) {
        Ok(file) => file,
        Err(e) => {
            eprintln!("Error reading file '{}': {}", name, e);
            std::process::exit(1);
        }
    };

    let mut sources = SourceCache::new();
    let mut resolver = DefaultIncludeResolver::default();

    let ast = match sources.parse_with_includes(&name, &src, &mut resolver) {
        Ok(ast) => ast,
        Err(e) => {
            e.print(&sources).expect("Failed to print error");
            std::process::exit(1);
        }
    };

    if args.debug {
        println!("PARSED: {:?}", debug.elapsed());
        debug = std::time::Instant::now();
    }

    let script = match Compiler::compile(&ast) {
        Ok(script) => script,
        Err(e) => {
            e.print(&sources).expect("Failed to print error");
            std::process::exit(1);
        }
    };

    if args.debug {
        println!("COMPILED: {:?}", debug.elapsed());
        debug = std::time::Instant::now();
    }

    let mut opts = VmOptions::default();

    if let Some(max_runtime) = args.max_runtime {
        opts.max_runtime = std::time::Duration::from_millis(max_runtime);
    }

    if let Some(stack_size) = args.stack_size {
        opts.stack_size = stack_size;
    }

    if let Some(call_stack_size) = args.call_stack_size {
        opts.call_stack_size = call_stack_size;
    }

    if let Some(string_max_size) = args.string_max_size {
        opts.string_max_size = string_max_size;
    }

    if let Some(array_max_size) = args.array_max_size {
        opts.array_max_size = array_max_size;
    }

    if args.disassemble {
        let dis = Disassembler::new(&script, &sources);

        if args.debug {
            println!("DISASSEMBLED: {:?}", debug.elapsed());
        }

        for line in dis.disassemble() {
            println!("{}", line);
        }
    }

    let mut vm = VM::new((), script, opts);

    vm.add_builtins(qstd::stdio);

    let timeit = if args.timeit {
        Some(std::time::Instant::now())
    } else {
        None
    };

    let res = vm.run();
    if let Some(start) = timeit {
        println!("TOOK: {:?}", start.elapsed());
    }

    match res {
        Ok(value) => {
            if let Value::None = value {
                std::process::exit(0);
            }

            if args.result {
                println!("RETURNED: {}", value);
            }
            std::process::exit(0);
        }
        Err(e) => {
            vm.trace_back()
                .print(&sources)
                .expect("Failed to print error");
            e.print(&sources).expect("Failed to print error");
            std::process::exit(1);
        }
    }
}
