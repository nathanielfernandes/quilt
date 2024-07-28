pub mod bluff;
pub mod error;
pub mod pool;
pub mod rlevec;
pub mod sourcecache;
pub mod span;
pub mod vecc;

#[cfg(not(feature = "arc"))]
pub type Rc<T> = std::rc::Rc<T>;

#[cfg(not(feature = "arc"))]
pub type RefCell<T> = std::cell::RefCell<T>;

#[cfg(feature = "arc")]
pub type Rc<T> = std::sync::Arc<T>;

#[cfg(feature = "arc")]
pub type RefCell<T> = atomic_refcell::AtomicRefCell<T>;

pub use fxhash::{FxHashMap, FxHashSet};
