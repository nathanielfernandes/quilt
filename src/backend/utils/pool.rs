/// A pool of values that can be indexed by a `PoolID`.
///
/// This is useful for storing values that are used multiple times in a program.
#[derive(Debug, Clone)]
pub struct Pool<V, ID> {
    pub entries: Vec<V>,
    duplicates: fxhash::FxHashMap<V, ID>,
}

impl<V, ID> Pool<V, ID>
where
    ID: Copy + PoolID + Into<usize>,
    V: Eq + std::hash::Hash + Clone,
{
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
            duplicates: fxhash::FxHashMap::default(),
        }
    }

    pub fn take(mut self) -> Vec<V> {
        std::mem::take(&mut self.entries)
    }

    pub fn take_inplace(mut self, to: &mut Vec<V>) {
        std::mem::swap(&mut self.entries, to);
    }

    pub fn clear(&mut self) {
        self.entries.clear();
        self.duplicates.clear();
    }

    /// Add a value to the pool.
    /// If the value already exists in the pool, the existing ID is returned.
    /// Otherwise, the value is added to the pool and a new ID is returned.
    /// The ID can be used to retrieve the value from the pool.
    /// The ID is guaranteed to be unique for the lifetime of the pool.
    pub fn add(&mut self, value: V) -> ID {
        if let Some(id) = self.duplicates.get(&value) {
            *id
        } else {
            let id = ID::from_usize(self.entries.len());
            self.entries.push(value.clone());
            self.duplicates.insert(value, id);
            id
        }
    }

    // Add a value to the pool.
    // forces the value to be added to the pool, even if it already exists.
    // The ID can be used to retrieve the value from the pool.
    pub fn add_force(&mut self, value: V) -> ID {
        let id = ID::from_usize(self.entries.len());
        self.entries.push(value.clone());
        self.duplicates.insert(value, id);
        id
    }

    pub fn add_ref(&mut self, value: &V) -> ID {
        if let Some(id) = self.duplicates.get(value) {
            *id
        } else {
            let id = ID::from_usize(self.entries.len());
            self.entries.push(value.clone());
            self.duplicates.insert(value.clone(), id);
            id
        }
    }

    pub fn add_ref_force(&mut self, value: &V) -> ID {
        let id = ID::from_usize(self.entries.len());
        self.entries.push(value.clone());
        self.duplicates.insert(value.clone(), id);
        id
    }

    pub fn get(&self, id: ID) -> Option<&V> {
        self.entries.get(id.into())
    }

    pub fn get_unchecked(&self, id: ID) -> &V {
        &self.entries[id.into()]
    }

    pub fn get_mut(&mut self, id: ID) -> Option<&mut V> {
        self.entries.get_mut(id.into())
    }

    pub fn get_mut_unchecked(&mut self, id: ID) -> &mut V {
        &mut self.entries[id.into()]
    }

    pub fn update(&mut self, id: ID, value: V) {
        self.entries[id.into()] = value;
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = &V> {
        self.entries.iter()
    }
}

pub trait PoolID: Sized {
    fn from_usize(id: usize) -> Self;
}

macro_rules! impl_poolable_id {
    ($($t:ty),*) => {
        $(
            impl PoolID for $t {
                fn from_usize(id: usize) -> Self {
                    id as Self
                }
            }
        )*
    };
}

impl_poolable_id!(u8, u16, u32, u64, u128, usize);

#[test]
fn test_pool() {
    let mut pool: Pool<String, u16> = Pool::new();

    assert_eq!(pool.add("hello".to_string()), 0);
    assert_eq!(pool.add("hello".to_string()), 0);
    assert_eq!(pool.add("world".to_string()), 1);
    assert_eq!(pool.get(0), Some(&"hello".to_string()));
    assert_eq!(pool.len(), 2);

    pool.update(0, "hello world".to_string());

    assert_eq!(pool.get(0), Some(&"hello world".to_string()));
    assert_eq!(pool.len(), 2);

    let mut pool: Pool<char, u16> = Pool::new();

    assert_eq!(pool.add('a'), 0);
    assert_eq!(pool.add('a'), 0);
    assert_eq!(pool.add('b'), 1);
    assert_eq!(pool.get(0), Some(&'a'));
    assert_eq!(pool.len(), 2);

    pool.update(0, 'c');

    assert_eq!(pool.get(0), Some(&'c'));
    assert_eq!(pool.len(), 2);
}
