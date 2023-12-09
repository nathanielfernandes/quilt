use crate::{
    error::{Error, OverflowError},
    Rc,
};

pub trait GetSize {
    fn get_size(&self) -> usize {
        std::mem::size_of_val(self)
    }
}

macro_rules! impl_get_size_static {
    ($($t:ty),*) => {
        $(
            impl GetSize for $t {
                fn get_size(&self) -> usize {
                    std::mem::size_of::<$t>()
                }
            }
        )*
    };
}

impl_get_size_static!(u8, u16, u32, u64, u128, usize, i8, i16, i32, i64, i128, isize, bool, char);

impl GetSize for String {
    #[inline]
    fn get_size(&self) -> usize {
        self.len()
    }
}

impl<T: GetSize> GetSize for Vec<T> {
    #[inline]
    fn get_size(&self) -> usize {
        self.iter().map(|x| x.get_size()).sum()
    }
}

impl<T: GetSize, const N: usize> GetSize for [T; N] {
    #[inline]
    fn get_size(&self) -> usize {
        self.iter().map(|x| x.get_size()).sum()
    }
}

impl<T: GetSize> GetSize for Option<T> {
    #[inline]
    fn get_size(&self) -> usize {
        match self {
            Some(x) => x.get_size(),
            None => 0,
        }
    }
}

impl<T: GetSize> GetSize for &[T] {
    #[inline]
    fn get_size(&self) -> usize {
        self.iter().map(|x| x.get_size()).sum()
    }
}

impl<T: GetSize> GetSize for Rc<T> {
    #[inline]
    fn get_size(&self) -> usize {
        self.as_ref().get_size()
    }
}
impl<T: GetSize> GetSize for Box<T> {
    #[inline]
    fn get_size(&self) -> usize {
        self.as_ref().get_size()
    }
}

// A vector with a fixed max memory size.
// (default is 256kb)

#[derive(Debug, Clone, Hash)]
pub struct Vecc<T>
where
    T: GetSize,
{
    data: Vec<T>,
    size: usize,
    max_size: usize,
}

impl<T> PartialEq for Vecc<T>
where
    T: PartialEq + GetSize,
{
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data
    }
}

impl<T> Vecc<T>
where
    T: GetSize,
{
    pub fn new(max_size: usize) -> Self {
        Self {
            data: Vec::new(),
            size: 0,
            max_size,
        }
    }

    pub fn force_new(data: Vec<T>) -> Self {
        let s = data.iter().map(|x| x.get_size()).sum();
        Self {
            size: s,
            data,
            max_size: s,
        }
    }

    pub fn with_capacity(capacity: usize, max_size: usize) -> Self {
        Self {
            data: Vec::with_capacity(capacity),
            size: 0,
            max_size,
        }
    }

    pub fn try_new_from(data: Vec<T>, max_size: usize) -> Result<Self, Error> {
        let size = data.iter().map(|x| x.get_size()).sum();
        if size > max_size {
            return Err(OverflowError::ArrayTooLarge.into());
        }
        Ok(Self {
            data,
            size,
            max_size,
        })
    }

    pub fn new_from(data: Vec<T>, max_size: usize) -> Self {
        Self {
            size: data.iter().map(|x| x.get_size()).sum(),
            data,
            max_size,
        }
    }

    pub fn data(&self) -> &Vec<T> {
        &self.data
    }

    pub fn into_data(self) -> Vec<T> {
        self.data
    }

    pub fn max_size(&self) -> usize {
        self.max_size
    }

    pub fn size(&self) -> usize {
        self.size
    }

    pub fn to_vec(&self) -> Vec<T>
    where
        T: Clone,
    {
        self.data.to_vec()
    }

    pub fn push(&mut self, item: T) -> Result<(), Error> {
        let item_size = item.get_size();

        if self.size + item_size > self.max_size {
            return Err(OverflowError::ArrayTooLarge.into());
        }
        self.size += item_size;
        self.data.push(item);
        Ok(())
    }

    pub fn pop(&mut self) -> Option<T> {
        self.data.pop().map(|item| {
            self.size -= item.get_size();
            item
        })
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.data.iter()
    }

    pub fn into_iter(self) -> impl Iterator<Item = T> {
        self.data.into_iter()
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        self.data.get(index)
    }

    pub fn set(&mut self, index: usize, item: T) -> Result<(), Error> {
        let item_size = item.get_size();
        if self.size + item_size > self.max_size {
            return Err(OverflowError::ArrayTooLarge.into());
        }
        self.size += item_size;

        self.data[index] = item;
        Ok(())
    }

    pub fn clear(&mut self) {
        self.data.clear();
        self.size = 0;
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    pub fn last(&self) -> Option<&T> {
        self.data.last()
    }

    pub fn remove(&mut self, index: usize) -> T {
        let item = self.data.remove(index);
        self.size -= item.get_size();

        item
    }

    pub fn insert(&mut self, index: usize, item: T) -> Result<(), Error> {
        let item_size = item.get_size();
        if self.size + item_size > self.max_size {
            return Err(OverflowError::ArrayTooLarge.into());
        }
        self.size += item_size;
        self.data.insert(index, item);
        Ok(())
    }

    pub fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) -> Result<(), Error> {
        for item in iter {
            self.size += item.get_size();
            if self.size > self.max_size {
                return Err(OverflowError::ArrayTooLarge.into());
            }

            self.data.push(item);
        }

        Ok(())
    }

    pub fn swap_remove(&mut self, index: usize) -> T {
        let item = self.data.swap_remove(index);
        self.size -= item.get_size();

        item
    }

    pub fn truncate(&mut self, len: usize) {
        let mut size = 0;
        for item in self.data.drain(len..) {
            size += item.get_size();
        }
        self.size -= size;
    }
}

impl<T: GetSize> GetSize for Vecc<T> {
    #[inline]
    fn get_size(&self) -> usize {
        self.size
    }
}

//impl index traits
impl<T> std::ops::Index<usize> for Vecc<T>
where
    T: GetSize,
{
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        &self.data[index]
    }
}
