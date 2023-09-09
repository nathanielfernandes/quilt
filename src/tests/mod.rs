#![cfg(test)]

use crate::prelude::*;

fn test_code<const SS: usize, const CSS: usize>(
    test_name: &str,
    src: &str,
) -> Result<(), &'static str> {
    let mut sources = SourceCache::new();
    let ast = match sources.parse_with_includes(
        test_name,
        &src,
        &mut DefaultIncludeResolver::default(),
    ) {
        Ok(ast) => ast,
        Err(e) => {
            e.print(&sources).expect("failed to print error");
            return Err(e.0.name());
        }
    };

    let script = match Compiler::compile(&ast) {
        Ok(script) => script,
        Err(e) => {
            e.print(&sources).expect("failed to print error");
            return Err(e.0.name());
        }
    };

    let mut vm = VM::new(
        (),
        script,
        VmOptions::build()
            .with_stack_size(SS)
            .with_call_stack_size(CSS),
    );
    vm.add_builtins(qstd::stdio);
    vm.add_builtins(crate::prelude::qstd::testing);

    if let Err(e) = vm.run() {
        e.print(&sources).expect("failed to print error");
        return Err(e.0.name());
    }

    return Ok(());
}

#[test]
fn arith() -> Result<(), &'static str> {
    let src = include_str!("arith.ql");
    test_code::<20, 1>("arith.ql", src)?;

    Ok(())
}

#[test]
fn conditionals() -> Result<(), &'static str> {
    let src = include_str!("conds.ql");
    test_code::<20, 1>("conds.ql", src)?;

    Ok(())
}

#[test]
fn closure() -> Result<(), &'static str> {
    let src = include_str!("closure.ql");
    test_code::<64, 5>("closure.ql", src)?;
    Ok(())
}

#[test]
fn loops() -> Result<(), &'static str> {
    let src = include_str!("loops.ql");
    test_code::<20, 2>("loops.ql", src)?;

    Ok(())
}

#[test]
fn recursion() -> Result<(), &'static str> {
    let src = include_str!("recursion.ql");
    test_code::<64, 64>("recursion.ql", src)?;
    Ok(())
}

#[test]
fn bitwise() -> Result<(), &'static str> {
    let src = include_str!("bitwise.ql");
    test_code::<64, 64>("bitwise.ql", src)?;
    Ok(())
}
