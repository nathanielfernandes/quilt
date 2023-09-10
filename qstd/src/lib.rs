pub mod core;
pub mod io;
pub mod math;
pub mod strings;
pub mod testing;
pub mod time;

pub use core::*;
pub use io::*;
pub use math::*;
pub use strings::*;
pub use testing::testing;
pub use time::*;

use interpreter::{builtins::VmData, vm::VM};

#[inline]
pub fn std<Data>(vm: &mut VM<Data>)
where
    Data: VmData,
{
    vm.add_builtins(core);
    vm.add_builtins(math);
    vm.add_builtins(strings);
    vm.add_builtins(testing);
    vm.add_builtins(time);
}

#[inline]
pub fn stdio<Data>(vm: &mut VM<Data>)
where
    Data: VmData,
{
    vm.add_builtins(std);
    vm.add_builtins(io);
}
