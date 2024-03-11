use core::{
    borrow::{Borrow, BorrowMut},
    hash::Hash,
    mem::{ManuallyDrop, MaybeUninit},
    ops::{Deref, DerefMut, Index, IndexMut},
    ptr,
    slice::SliceIndex,
};

use super::Unsigned;

#[repr(transparent)]
pub struct Array<T, N: Unsigned>(N::Array<T>);

pub struct SimpleArrayVec<T, N: Unsigned> {
    array: Array<MaybeUninit<T>, N>,
    len: usize,
}

const unsafe fn transmute_unchecked<T, U>(t: T) -> U {
    #[repr(C)]
    union Trans<T, U> {
        t: ManuallyDrop<T>,
        u: ManuallyDrop<U>,
    }

    assert!(core::mem::size_of::<T>() == core::mem::size_of::<U>());

    unsafe {
        ManuallyDrop::into_inner(
            Trans {
                t: ManuallyDrop::new(t),
            }
            .u,
        )
    }
}

impl<T, N: Unsigned> Drop for SimpleArrayVec<T, N> {
    fn drop(&mut self) {
        let ptr = self.array.as_mut_ptr();
        unsafe { ptr::slice_from_raw_parts_mut(ptr, self.len).drop_in_place() }
    }
}

impl<T, N: Unsigned> SimpleArrayVec<T, N> {
    pub const fn new() -> Self {
        Self {
            array: Array::uninit(),
            len: 0,
        }
    }

    pub fn push(&mut self, value: T) {
        self.array[self.len] = MaybeUninit::new(value);
        self.len += 1;
    }

    pub unsafe fn push_unchecked(&mut self, value: T) {
        *self.array.get_unchecked_mut(self.len) = MaybeUninit::new(value);
        self.len += 1;
    }

    pub fn into_array(self) -> Array<T, N> {
        assert!(self.len == N::TO_USIZE);
        unsafe { self.into_array_unchecked() }
    }

    pub unsafe fn into_array_unchecked(self) -> Array<T, N> {
        let m = ManuallyDrop::new(self);
        unsafe { core::ptr::read(&m.array).assume_init() }
    }
}

impl<T, N: Unsigned> Array<T, N> {
    #[inline]
    #[allow(clippy::uninit_assumed_init)]
    pub const fn uninit() -> Array<MaybeUninit<T>, N> {
        unsafe { core::mem::MaybeUninit::uninit().assume_init() }
    }

    #[inline]
    #[allow(clippy::uninit_assumed_init)]
    pub const fn from_array<const L: usize>(array: [T; L]) -> Array<T, N>
    where
        T: Copy,
    {
        assert!(L == N::TO_USIZE);

        unsafe { transmute_unchecked(array) }
    }

    #[inline]
    pub const fn as_ptr(&self) -> *const T {
        self as *const Self as *const T
    }

    #[inline]
    pub fn as_mut_ptr(&mut self) -> *mut T {
        self as *mut Self as *mut T
    }

    #[inline]
    pub const fn len(&self) -> usize {
        N::TO_USIZE
    }

    #[inline]
    pub const fn as_slice(&self) -> &[T] {
        unsafe { core::slice::from_raw_parts(self.as_ptr(), N::TO_USIZE) }
    }

    #[inline]
    pub fn as_slice_mut(&mut self) -> &mut [T] {
        unsafe { core::slice::from_raw_parts_mut(self.as_mut_ptr(), N::TO_USIZE) }
    }
}

impl<T, N: Unsigned> Array<MaybeUninit<T>, N> {
    /// # Safety
    ///
    /// All elements of this array must be initialized
    pub const unsafe fn assume_init(self) -> Array<T, N> {
        transmute_unchecked(self)
    }
}

impl<T, I: SliceIndex<[T]>, N: Unsigned> Index<I> for Array<T, N> {
    type Output = I::Output;

    fn index(&self, index: I) -> &Self::Output {
        &self.as_slice()[index]
    }
}

impl<T, I: SliceIndex<[T]>, N: Unsigned> IndexMut<I> for Array<T, N> {
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        &mut self.as_slice_mut()[index]
    }
}

impl<T, N: Unsigned> Deref for Array<T, N> {
    type Target = [T];

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}

impl<T, N: Unsigned> DerefMut for Array<T, N> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_slice_mut()
    }
}

impl<T, N: Unsigned> AsRef<[T]> for Array<T, N> {
    #[inline]
    fn as_ref(&self) -> &[T] {
        self
    }
}

impl<T, N: Unsigned> AsMut<[T]> for Array<T, N> {
    #[inline]
    fn as_mut(&mut self) -> &mut [T] {
        self
    }
}

impl<T, N: Unsigned> Borrow<[T]> for Array<T, N> {
    #[inline]
    fn borrow(&self) -> &[T] {
        self
    }
}

impl<T, N: Unsigned> BorrowMut<[T]> for Array<T, N> {
    #[inline]
    fn borrow_mut(&mut self) -> &mut [T] {
        self
    }
}

impl<T: Eq, N: Unsigned> Eq for Array<T, N> {}
impl<T: PartialEq, N: Unsigned, U: Borrow<[T]>> PartialEq<U> for Array<T, N> {
    fn eq(&self, other: &U) -> bool {
        self.as_slice() == other.borrow()
    }
}

impl<T: PartialOrd, N: Unsigned, U: Borrow<[T]>> PartialOrd<U> for Array<T, N> {
    fn partial_cmp(&self, other: &U) -> Option<core::cmp::Ordering> {
        self.as_slice().partial_cmp(other.borrow())
    }
}

impl<T: Ord, N: Unsigned> Ord for Array<T, N> {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.as_slice().cmp(other.borrow())
    }
}

impl<T: Hash, N: Unsigned> Hash for Array<T, N> {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.as_slice().hash(state);
    }
}

// impl<T: Copy, N: Unsigned> Copy for Array<T, N> {}
impl<T: Clone, N: Unsigned> Clone for Array<T, N> {
    fn clone(&self) -> Self {
        let mut v = SimpleArrayVec::<T, N>::new();

        for i in self.iter() {
            unsafe { v.push_unchecked(i.clone()) }
        }

        unsafe { v.into_array_unchecked() }
    }
}

impl<T: core::fmt::Debug, N: Unsigned> core::fmt::Debug for Array<T, N> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.as_slice().fmt(f)
    }
}
