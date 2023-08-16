pub mod core;
pub mod io;
pub mod math;
pub mod strings;

pub use core::*;
pub use io::*;
pub use math::*;
pub use strings::*;

use interpreter::{builtins::VmData, vm::VM};

#[inline]
pub fn std<Data>(vm: &mut VM<Data>)
where
    Data: VmData,
{
    vm.add_builtins(core);
    vm.add_builtins(math);
    vm.add_builtins(strings);
}

#[inline]
pub fn stdio<Data>(vm: &mut VM<Data>)
where
    Data: VmData,
{
    vm.add_builtins(std);
    vm.add_builtins(io);
}
