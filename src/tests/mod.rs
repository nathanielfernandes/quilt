#![cfg(test)]

use crate::prelude::*;

fn test_code<const SS: usize, const CSS: usize>(
    test_name: &str,
    src: &str,
) -> Result<Value, ErrorS> {
    let mut sources = SourceCache::new();
    let ast =
        sources.parse_with_includes(test_name, &src, &mut DefaultIncludeResolver::default())?;

    let script = Compiler::compile(&ast)?;

    let mut vm = VM::new(
        (),
        script,
        VmOptions::build()
            .with_stack_size(SS)
            .with_call_stack_size(CSS),
    );
    vm.add_builtins(qstd::io);
    vm.add_builtins(qstd::math);
    vm.add_builtins(qstd::core);

    vm.run()
}

#[test]
fn arith() -> Result<(), ErrorS> {
    let src = include_str!("arith.ql");
    test_code::<20, 1>("arith.ql", src)?;

    Ok(())
}

#[test]
fn conditionals() -> Result<(), ErrorS> {
    let src = include_str!("conds.ql");
    test_code::<20, 1>("conds.ql", src)?;

    Ok(())
}

#[test]
fn closure() -> Result<(), ErrorS> {
    let src = include_str!("closure.ql");
    test_code::<64, 5>("closure.ql", src)?;
    Ok(())
}

#[test]
fn loops() -> Result<(), ErrorS> {
    let src = include_str!("loops.ql");
    test_code::<20, 2>("loops.ql", src)?;

    Ok(())
}

#[test]
fn recursion() -> Result<(), ErrorS> {
    let src = include_str!("recursion.ql");
    test_code::<64, 64>("recursion.ql", src)?;
    Ok(())
}
