// heap struct that contains all the heap objects in a vec
// every new object is either pushed to the end of the vec or inserted at an
// available index, which is stored in the free list

pub type HeapId = usize;

pub struct Heap<T> {
    data: Vec<T>,
    free: Vec<HeapId>,
}

impl<T> Heap<T>
where
    T: Default,
{
    pub fn new() -> Self {
        Self {
            data: Vec::new(),
            free: Vec::new(),
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            data: Vec::with_capacity(capacity),
            free: Vec::with_capacity(capacity),
        }
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    // allocate a new object on the heap
    // if there is an available index in the free list, use that
    // otherwise, push the object to the end of the vec
    pub fn allocate(&mut self, obj: T) -> HeapId {
        if let Some(id) = self.free.pop() {
            self.data[id] = obj;
            id
        } else {
            self.data.push(obj);
            self.data.len() - 1
        }
    }

    // free an object on the heap
    // add the index to the free list
    // Note: this does not actually remove the object from the vec
    pub fn free(&mut self, id: HeapId) {
        if let Some(value) = self.get_mut(id) {
            *value = T::default();
        }

        self.free.push(id);
    }

    pub fn get(&self, id: HeapId) -> Option<&T> {
        self.data.get(id)
    }

    pub fn get_mut(&mut self, id: HeapId) -> Option<&mut T> {
        self.data.get_mut(id)
    }

    pub fn clear(&mut self) {
        self.data.clear();
        self.free.clear();
    }
}

#[test]
fn test_heap() {
    let mut heap: Heap<i32> = Heap::with_capacity(10);

    let id1 = heap.allocate(1);
    let id2 = heap.allocate(2);
    let id3 = heap.allocate(3);

    assert_eq!(heap.get(id1), Some(&1));
    assert_eq!(heap.get(id2), Some(&2));
    assert_eq!(heap.get(id3), Some(&3));

    heap.free(id2);
    let id4 = heap.allocate(4);
    let id5 = heap.allocate(5);

    assert_eq!(id4, id2);
    assert_eq!(id5, heap.len() as HeapId - 1);
}
