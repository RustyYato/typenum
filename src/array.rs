use std::{
    mem::{ManuallyDrop, MaybeUninit},
    ops::{Deref, DerefMut, Index, IndexMut},
    slice::SliceIndex,
};

use super::Unsigned;

pub struct Array<T, N: Unsigned>(N::Array<T>);

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

        #[repr(C)]
        union Trans<T, U> {
            t: ManuallyDrop<T>,
            u: ManuallyDrop<U>,
        }

        unsafe {
            ManuallyDrop::into_inner(
                Trans {
                    t: ManuallyDrop::new(array),
                }
                .u,
            )
        }
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
