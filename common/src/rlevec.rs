/// A run-length encoded vector.
///
/// This is useful for storing a vector of values that are mostly the same.
/// For example, a vector of 1000 `0`s can be stored as a single `0` and a `1000` run.
#[derive(Debug, Clone, PartialEq)]
pub struct RleVec<T>(Vec<(usize, T)>);

impl<T> RleVec<T>
where
    T: PartialEq,
{
    #[inline]
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn push(&mut self, value: T) {
        if let Some((count, last)) = self.0.last_mut() {
            if *last == value {
                *count += 1;
                return;
            }
        }

        self.0.push((1, value));
    }

    pub fn push_n(&mut self, value: T, count: usize) {
        if let Some((last_count, last)) = self.0.last_mut() {
            if *last == value {
                *last_count += count;
                return;
            }
        }

        self.0.push((count, value));
    }

    pub fn last(&self) -> Option<&T> {
        self.0.last().map(|(_, value)| value)
    }

    pub fn first(&self) -> Option<&T> {
        self.0.first().map(|(_, value)| value)
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        let mut index = index;
        for (count, value) in &self.0 {
            if index < *count {
                return Some(value);
            }

            index -= count;
        }

        None
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.0.iter().map(|(count, _)| count).sum()
    }

    #[inline]
    pub fn data_len(&self) -> usize {
        self.0.len()
    }
}

impl<T> std::ops::Index<usize> for RleVec<T>
where
    T: PartialEq,
{
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        self.get(index).expect("index out of bounds")
    }
}

#[test]
fn test_rlevec() {
    let mut rle = RleVec::new();
    rle.push(1);
    rle.push(1);
    rle.push(1);
    rle.push(2);
    rle.push(2);

    assert_eq!(rle.get(0), Some(&1));
    assert_eq!(rle.get(1), Some(&1));
    assert_eq!(rle.get(2), Some(&1));
    assert_eq!(rle.get(3), Some(&2));
    assert_eq!(rle.get(4), Some(&2));
    assert_eq!(rle.get(5), None);

    assert_eq!(rle[0], 1);
    assert_eq!(rle[1], 1);

    assert_eq!(rle.len(), 5);
    assert_eq!(rle.data_len(), 2);
}
