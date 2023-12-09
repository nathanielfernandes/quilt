pub struct Bluff<T> {
    data: Vec<T>,
    idx: usize,
}

impl<T> Bluff<T> {
    pub fn new() -> Self {
        Self {
            data: Vec::new(),
            idx: 0,
        }
    }

    pub fn with_capacity(cap: usize) -> Self {
        Self {
            data: Vec::with_capacity(cap),
            idx: 0,
        }
    }

    #[inline]
    pub fn clear(&mut self) {
        self.idx = 0;
    }

    #[inline]
    pub fn push(&mut self, value: T) {
        if self.idx == self.data.len() {
            self.data.push(value);
        } else {
            self.data[self.idx] = value;
        }
        self.idx += 1;
    }

    #[inline]
    pub fn init(&mut self, value: T) {
        self.idx = 0;
        self.push(value);
    }

    #[inline]
    pub fn pop(&mut self) -> Option<&T> {
        if self.idx == 0 {
            None
        } else {
            self.idx -= 1;
            Some(&self.data[self.idx])
        }
    }

    #[inline]
    pub fn peek(&self) -> Option<&T> {
        if self.idx == 0 {
            None
        } else {
            Some(&self.data[self.idx - 1])
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.idx
    }

    pub fn drain(&mut self) -> impl Iterator<Item = &T> {
        let idx = self.idx;
        self.idx = 0;
        self.data[..idx].iter()
    }
}

#[test]
fn bluff() {
    let mut bluff = Bluff::new();

    bluff.push(2);

    assert_eq!(bluff.peek(), Some(&2));

    bluff.push(3);

    assert_eq!(bluff.peek(), Some(&3));

    assert_eq!(bluff.pop(), Some(&3));

    assert_eq!(bluff.peek(), Some(&2));

    bluff.push(4);

    assert_eq!(bluff.peek(), Some(&4));

    bluff.clear();

    assert_eq!(bluff.peek(), None);

    bluff.push(5);

    assert_eq!(bluff.peek(), Some(&5));

    bluff.push(6);
    bluff.push(7);
    bluff.push(8);

    bluff.init(2);

    let mut iter = bluff.drain();

    assert_eq!(iter.next(), Some(&2));
    assert_eq!(iter.next(), None);

    // test drain
    let mut bluff = Bluff::new();

    bluff.push(2);
    bluff.push(3);
    bluff.push(4);

    let mut iter = bluff.drain();

    assert_eq!(iter.next(), Some(&2));
    assert_eq!(iter.next(), Some(&3));
    assert_eq!(iter.next(), Some(&4));
}
