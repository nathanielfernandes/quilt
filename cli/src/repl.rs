use std::io::Write;

use quilt::prelude::{builtins::BuiltinAdderFn, *};

pub struct Repl<const N: usize, Data = ()>
where
    Data: VmData,
{
    vm: VM<Data>,
    compiler: Compiler,
    sources: SourceCache,

    c: usize,

    builtins: [BuiltinAdderFn<Data>; N],
}

impl<const N: usize, Data> Repl<N, Data>
where
    Data: VmData,
{
    pub fn new(data: Data, options: VmOptions, builtins: [BuiltinAdderFn<Data>; N]) -> Self {
        let compiler = Compiler::new("@__repl__".into());
        let script = compiler.clone_as_script();
        Self {
            vm: VM::new(data, script, options),
            compiler,
            sources: SourceCache::new(),

            builtins,

            c: 0,
        }
    }

    pub fn execute(&mut self, source: &str) -> Result<Value, ErrorS> {
        let name = format!("@__exec__{}", self.c);
        self.c += 1;

        let ast = self.sources.parse_with_includes(
            &name,
            source,
            &mut DefaultIncludeResolver::default(),
        )?;

        self.compiler.compile_stmnts(&ast)?;

        let script = self.compiler.clone_as_script();

        self.compiler.reset_state("@__repl__".into());
        self.vm.update_script(script);

        for builtin in self.builtins.iter() {
            builtin(&mut self.vm);
        }

        self.vm.run()
    }

    pub fn start(&mut self) {
        println!("Quilt REPL");

        loop {
            print!(">>> ");
            std::io::stdout().flush().expect("Failed to flush stdout");

            let mut input = String::new();
            std::io::stdin()
                .read_line(&mut input)
                .expect("Failed to read line");

            let input = input.trim();

            if input.is_empty() {
                continue;
            }

            match self.execute(input) {
                Ok(value) => {
                    if let Value::None = value {
                        continue;
                    }

                    println!("{}", value);
                }
                Err(e) => {
                    self.vm
                        .trace_back()
                        .print(&self.sources)
                        .expect("Failed to print error");
                    e.print(&self.sources).expect("Failed to print error")
                }
            }
        }
    }
}
