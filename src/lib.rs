#![no_std]
#![allow(type_alias_bounds)]
#![recursion_limit = "256"]

pub mod consts;

#[cfg(test)]
mod tests;

pub mod array;

#[derive(Clone, Copy)]
pub struct IZeros;
#[derive(Clone, Copy)]
pub struct IOnes;
#[derive(Clone, Copy)]
pub struct B0;
#[derive(Clone, Copy)]
pub struct B1;
#[derive(Clone, Copy)]
pub struct Int<M, L> {
    _most_significant_bits: M,
    _least_significant_bit: L,
}

#[derive(Clone, Copy)]
pub struct PZ;
#[derive(Clone, Copy)]
pub struct PS<P>(P);

use seal::Seal;
mod seal {
    pub trait Seal {}
}

pub trait Ordering: Seal + div_private::DivLoopOrd {
    type Then<O: Ordering>: Ordering;
    type Reverse: Ordering<Reverse = Self>;
    type PickInt<L: Integer, E: Integer, G: Integer>: Integer;
    type PickUInt<L: Unsigned, E: Unsigned, G: Unsigned>: Unsigned;
    type ToSignum: Signum;
}

#[derive(Clone, Copy)]
pub struct OrdLess;
#[derive(Clone, Copy)]
pub struct OrdEq;
#[derive(Clone, Copy)]
pub struct OrdGreater;

impl Seal for OrdLess {}
impl Ordering for OrdLess {
    type Then<O: Ordering> = Self;
    type Reverse = OrdGreater;
    type PickInt<L: Integer, E: Integer, G: Integer> = L;
    type PickUInt<L: Unsigned, E: Unsigned, G: Unsigned> = L;
    type ToSignum = consts::N1;
}
impl Seal for OrdEq {}
impl Ordering for OrdEq {
    type Then<O: Ordering> = O;
    type Reverse = Self;
    type PickInt<L: Integer, E: Integer, G: Integer> = E;
    type PickUInt<L: Unsigned, E: Unsigned, G: Unsigned> = E;
    type ToSignum = consts::Z0;
}
impl Seal for OrdGreater {}
impl Ordering for OrdGreater {
    type Then<O: Ordering> = Self;
    type Reverse = OrdLess;
    type PickInt<L: Integer, E: Integer, G: Integer> = G;
    type PickUInt<L: Unsigned, E: Unsigned, G: Unsigned> = G;
    type ToSignum = consts::P1;
}

pub trait Bit: Seal {
    const TO_BOOL: bool;

    const NEW: Self;
    // If then else, if the bit is true then T is returned else F is returned
    type Ite<T: Bit, F: Bit>: Bit;
    type PickInt<T: Integer, F: Integer>: Integer;
    type PushOntoInt<I: Integer>: Integer<LeastSig = Self, MostSig = I, BitGet<PZ> = Self>;
    type MaybeInc<A: Integer>: Integer;
    type MaybeDec<A: Integer>: Integer;
    type Mul<A: Integer>: Integer;
    type Pow<A: Integer>: Integer;
    type AddPeano<P: Peano>: Peano;

    type Not: Bit<Not = Self>;
    type CompareZero: Ordering;
    type MaybeItem<T>;
}

impl Seal for B0 {}
impl Bit for B0 {
    const TO_BOOL: bool = false;

    const NEW: Self = Self;
    type Ite<T: Bit, F: Bit> = F;
    type PickInt<T: Integer, F: Integer> = F;
    type PushOntoInt<I: Integer> = I::PushLeastZero;
    type MaybeInc<A: Integer> = A;
    type MaybeDec<A: Integer> = A::Dec;
    type Mul<A: Integer> = IZeros;
    type Pow<A: Integer> = consts::P1;
    type AddPeano<P: Peano> = P;

    type Not = B1;
    type CompareZero = OrdEq;
    type MaybeItem<T> = ();
}

impl Seal for B1 {}
impl Bit for B1 {
    const TO_BOOL: bool = true;

    const NEW: Self = Self;
    type Ite<T: Bit, F: Bit> = T;
    type PickInt<T: Integer, F: Integer> = T;
    type PushOntoInt<I: Integer> = I::PushLeastOne;
    type MaybeInc<A: Integer> = A::Inc;
    type MaybeDec<A: Integer> = A;
    type Mul<A: Integer> = A;
    type Pow<A: Integer> = A;
    type AddPeano<P: Peano> = PS<P>;

    type Not = B0;
    type CompareZero = OrdGreater;
    type MaybeItem<T> = T;
}

pub type BitAnd<A: Bit, B: Bit> = <A as Bit>::Ite<B, B0>;
pub type BitOr<A: Bit, B: Bit> = <A as Bit>::Ite<B1, B>;
pub type BitNot<A: Bit> = <A as Bit>::Not;
pub type BitXor<A: Bit, B: Bit> = <A as Bit>::Ite<BitNot<B>, B>;
pub type BitNand<A: Bit, B: Bit> = BitNot<BitAnd<A, B>>;
pub type BitNor<A: Bit, B: Bit> = BitNot<BitOr<A, B>>;
pub type BitNxor<A: Bit, B: Bit> = BitNot<BitXor<A, B>>;
pub type BitProd<A: Bit, B: Integer> = <A as Bit>::Mul<B>;

pub trait Peano: Seal + Sized + div_private::DivLoopPeano {
    type Add<B: Peano>: Peano;
    type Mul<B: Peano>: Peano;
    type Div2: Peano;
    type IncDiv2: Peano;
    type Mod2: Bit;
    type AsUnsigned: Unsigned;
    type CmpZero: Ordering;

    type Shl<A: Integer, Fill: Bit>: Integer;
    type Shr<A: Integer>: Integer;
    type BitGet<MostSig: Integer, LeastSig: Bit>: Bit;
    type BitSet<MostSig: Integer, LeastSig: Bit, B: Bit>: Integer;
}
impl Seal for PZ {}
impl Peano for PZ {
    type Add<B: Peano> = B;
    type Mul<B: Peano> = PZ;
    type Div2 = PZ;
    type IncDiv2 = PZ;
    type Mod2 = B0;
    type AsUnsigned = IZeros;
    type CmpZero = OrdEq;

    type Shl<A: Integer, Fill: Bit> = A;
    type Shr<A: Integer> = A;
    type BitGet<MostSig: Integer, LeastSig: Bit> = LeastSig;
    type BitSet<MostSig: Integer, LeastSig: Bit, B: Bit> = PushBit<MostSig, B>;
}
impl<P: Peano> Seal for PS<P> {}
impl<P: Peano> Peano for PS<P> {
    type Add<B: Peano> = PS<P::Add<B>>;
    type Mul<B: Peano> = B::Add<P::Mul<B>>;
    type Div2 = P::IncDiv2;
    type IncDiv2 = PS<P::Div2>;
    type Mod2 = BitNot<P::Mod2>;
    type AsUnsigned = Int<<Self::Div2 as Peano>::AsUnsigned, Self::Mod2>;
    type CmpZero = OrdGreater;

    type Shl<A: Integer, Fill: Bit> = PushBit<P::Shl<A, Fill>, Fill>;
    type Shr<A: Integer> = P::Shr<A::ShrOne>;
    type BitGet<MostSig: Integer, LeastSig: Bit> = MostSig::BitGet<P>;
    type BitSet<MostSig: Integer, LeastSig: Bit, B: Bit> = PushBit<MostSig::BitSet<P, B>, LeastSig>;
}

pub trait Signum: Integer {
    type Negate: Signum;
    type MulSignum<N: Signum>: Signum;
    type DivSignums<N: Signum>: SignumPair;
}

pub trait SignumPair: Seal {
    type Quot<Q: Integer, R: Unsigned>: Integer;
    type Rem<Q: Integer, R: Unsigned, D: Unsigned>: Unsigned;
}

pub trait Integer: Seal {
    const NEW: Self;
    type LeastSig: Bit;
    type MostSig: Integer;
    type PushLeastZero: Integer<LeastSig = B0, MostSig = Self, BitGet<PZ> = B0>;
    type PushLeastOne: Integer<LeastSig = B1, MostSig = Self, BitGet<PZ> = B1>;
    type Length: Peano;

    type Inc: Integer<LeastSig = BitNot<Self::LeastSig>>;
    type Dec: Integer<LeastSig = BitNot<Self::LeastSig>>;
    type Add<B: Integer>: Integer;
    type AddCarry<B: Integer, C: Bit>: Integer;
    type Mul<B: Integer>: Integer;
    type Pow<B: Unsigned>: Integer;
    type Neg: Integer;
    type Abs: Unsigned;
    type And<B: Integer>: Integer<
        LeastSig = BitAnd<Self::LeastSig, B::LeastSig>,
        MostSig = And<Self::MostSig, B::MostSig>,
    >;
    type Or<B: Integer>: Integer<
        LeastSig = BitOr<Self::LeastSig, B::LeastSig>,
        MostSig = Or<Self::MostSig, B::MostSig>,
    >;
    type Xor<B: Integer>: Integer<
        LeastSig = BitXor<Self::LeastSig, B::LeastSig>,
        MostSig = Xor<Self::MostSig, B::MostSig>,
    >;
    type Nand<B: Integer>: Integer<
        LeastSig = BitNand<Self::LeastSig, B::LeastSig>,
        MostSig = Nand<Self::MostSig, B::MostSig>,
    >;
    type Nor<B: Integer>: Integer<
        LeastSig = BitNor<Self::LeastSig, B::LeastSig>,
        MostSig = Nor<Self::MostSig, B::MostSig>,
    >;
    type Nxor<B: Integer>: Integer<
        LeastSig = BitNxor<Self::LeastSig, B::LeastSig>,
        MostSig = Nxor<Self::MostSig, B::MostSig>,
    >;
    type Not: Integer<
        Not = Self,
        LeastSig = BitNot<Self::LeastSig>,
        MostSig = <Self::MostSig as Integer>::Not,
        IsNeg = BitNot<Self::IsNeg>,
    >;
    type ShrOne: Integer<LeastSig = <Self::MostSig as Integer>::LeastSig>;

    type BitGet<P: Peano>: Bit;
    type BitSet<P: Peano, B: Bit>: Integer;

    type CompareZero: Ordering;
    type IsNeg: Bit;
    type ToUnsignedUnchecked: Unsigned;
}

impl Seal for IZeros {}
impl Integer for IZeros {
    const NEW: Self = Self;
    type LeastSig = B0;
    type MostSig = IZeros;

    type PushLeastZero = IZeros;
    type PushLeastOne = Int<IZeros, B1>;
    type Length = PZ;

    type Inc = Int<Self, B1>;
    type Dec = IOnes;
    type Add<B: Integer> = B;
    type AddCarry<B: Integer, C: Bit> = C::MaybeInc<B>;
    type Mul<B: Integer> = IZeros;
    type Pow<B: Unsigned> = IZeros;
    type Neg = IZeros;
    type Abs = IZeros;

    type And<B: Integer> = IZeros;
    type Or<B: Integer> = B;
    type Xor<B: Integer> = B;
    type Nand<B: Integer> = IOnes;
    type Nor<B: Integer> = B::Not;
    type Nxor<B: Integer> = B::Not;
    type Not = IOnes;
    type ShrOne = IZeros;
    type BitGet<P: Peano> = B0;
    type BitSet<P: Peano, B: Bit> = P::BitSet<Self, B0, B>;

    type CompareZero = OrdEq;
    type IsNeg = B0;
    type ToUnsignedUnchecked = Self;
}

impl Seal for IOnes {}
impl Integer for IOnes {
    const NEW: Self = Self;
    type LeastSig = B1;
    type MostSig = IOnes;

    type PushLeastZero = Int<IOnes, B0>;
    type PushLeastOne = IOnes;
    type Length = PZ;

    type Inc = IZeros;
    type Dec = Int<Self, B0>;
    type Add<B: Integer> = B::Dec;
    type AddCarry<B: Integer, C: Bit> = C::MaybeDec<B>;
    type Mul<B: Integer> = B::Neg;
    type Pow<B: Unsigned> = Sum<consts::P1, Prod<consts::P2, Neg<Int<IZeros, B::LeastSig>>>>;
    type Neg = Int<IZeros, B1>;
    type Abs = Int<IZeros, B1>;

    type And<B: Integer> = B;
    type Or<B: Integer> = IOnes;
    type Xor<B: Integer> = B::Not;
    type Nand<B: Integer> = B::Not;
    type Nor<B: Integer> = IZeros;
    type Nxor<B: Integer> = B;
    type Not = IZeros;
    type ShrOne = IOnes;
    type BitGet<P: Peano> = B1;
    type BitSet<P: Peano, B: Bit> = P::BitSet<Self, B1, B>;

    type CompareZero = OrdLess;
    type IsNeg = B1;
    type ToUnsignedUnchecked = IZeros;
}

impl<M: Integer, L: Bit> Seal for Int<M, L> {}
impl<M: Integer, L: Bit> Integer for Int<M, L> {
    const NEW: Self = Self {
        _least_significant_bit: Bit::NEW,
        _most_significant_bits: Integer::NEW,
    };
    type LeastSig = L;
    type MostSig = M;

    type PushLeastZero = Int<Self, B0>;
    type PushLeastOne = Int<Self, B1>;
    type Length = PS<M::Length>;

    type Inc = PushBit<L::PickInt<M::Inc, M>, BitNot<L>>;
    type Dec = PushBit<L::PickInt<M, M::Dec>, BitNot<L>>;
    type Add<B: Integer> =
        PushBit<SumCarry<M, B::MostSig, BitAnd<L, B::LeastSig>>, BitXor<L, B::LeastSig>>;
    type AddCarry<B: Integer, C: Bit> = PushBit<
        SumCarry<M, B::MostSig, AtLeastTwoBitsSet<L, B::LeastSig, C>>,
        BitXor<BitXor<L, B::LeastSig>, C>,
    >;
    // (2 m + l) * (2 bm + bl) =
    // (2 m * (2 bm + bl) + l * (2 bm + bl)) =
    // 4 m * bm + 2 m * bl + l * 2 bm + l * bl =
    // 4 m * bm + 2 (m * bl + l * bm) + l * bl
    // 2 * (2 m * bm + (m * bl + l * bm)) + l * bl
    // 2 * ((m * bl + l * bm) + 2 m * bm) + l * bl
    type Mul<B: Integer> = PushBit<
        // if both least significant bits are zero then this will be cheap
        Sum<Sum<BitProd<B::LeastSig, M>, BitProd<L, B::MostSig>>, PushBit<Prod<M, B::MostSig>, B0>>,
        BitAnd<L, B::LeastSig>,
    >;
    type Pow<B: Unsigned> = B::Exp<Self>;
    type Neg = <Self::Not as Integer>::Inc;

    // this ToUnsignedUnchecked is a no-op, but I can't remove it because Rust's type system is
    // too weak to figure this out
    type Abs =
        ToUnsignedUnchecked<<Self::CompareZero as Ordering>::PickInt<Self::Neg, IZeros, Self>>;

    type And<B: Integer> = PushBit<And<M, B::MostSig>, BitAnd<L, B::LeastSig>>;
    type Or<B: Integer> = PushBit<Or<M, B::MostSig>, BitOr<L, B::LeastSig>>;
    type Xor<B: Integer> = PushBit<Xor<M, B::MostSig>, BitXor<L, B::LeastSig>>;
    type Nand<B: Integer> = PushBit<Nand<M, B::MostSig>, BitNand<L, B::LeastSig>>;
    type Nor<B: Integer> = PushBit<Nor<M, B::MostSig>, BitNor<L, B::LeastSig>>;
    type Nxor<B: Integer> = PushBit<Nxor<M, B::MostSig>, BitNxor<L, B::LeastSig>>;
    type Not = Int<M::Not, BitNot<L>>;
    type ShrOne = M;
    type BitGet<P: Peano> = P::BitGet<M, L>;
    type BitSet<P: Peano, B: Bit> = P::BitSet<M, L, B>;

    type CompareZero = <M::CompareZero as Ordering>::Then<L::CompareZero>;
    type IsNeg = M::IsNeg;
    type ToUnsignedUnchecked = Int<M::ToUnsignedUnchecked, L>;
}

impl Signum for consts::N1 {
    type Negate = consts::P1;
    type MulSignum<N: Signum> = N::Negate;
    type DivSignums<N: Signum> = (Self, Self::MulSignum<N>);
}

impl Signum for consts::Z0 {
    type Negate = consts::Z0;
    type MulSignum<N: Signum> = consts::Z0;
    type DivSignums<N: Signum> = (consts::Z0, consts::Z0);
}

impl Signum for consts::P1 {
    type Negate = consts::N1;
    type MulSignum<N: Signum> = N;
    type DivSignums<N: Signum> = (Self, Self::MulSignum<N>);
}

impl Seal for (consts::Z0, consts::Z0) {}
impl SignumPair for (consts::Z0, consts::Z0) {
    type Quot<Q: Integer, R: Unsigned> = consts::Z0;
    type Rem<Q: Integer, R: Unsigned, D: Unsigned> = consts::Z0;
}

impl<S: Signum> Seal for (consts::P1, S) {}
impl<S: Signum> SignumPair for (consts::P1, S) {
    type Quot<Q: Integer, R: Unsigned> = Prod<Q, S>;
    type Rem<Q: Integer, R: Unsigned, D: Unsigned> = R;
}

impl<S: Signum> Seal for (consts::N1, S) {}
impl<S: Signum> SignumPair for (consts::N1, S) {
    type Quot<Q: Integer, R: Unsigned> = Prod<Sum<Q, SignumOf<R>>, S>;
    type Rem<Q: Integer, R: Unsigned, D: Unsigned> =
        <CmpZero<R> as Ordering>::PickUInt<consts::Z0, R, ToUnsignedUnchecked<Diff<D, R>>>;
}

// type AtLeastTwoBitsSet<A, B, C> = Or<Or<And<A, B>, And<A, C>>, And<B, C>>;
// (A & B) | (A & C) | (B & C)
// A & (B | C) | (B & C)
type AtLeastTwoBitsSet<A, B, C> = BitOr<BitAnd<A, BitOr<B, C>>, BitAnd<B, C>>;

type SumCarry<A: Integer, B: Integer, C: Bit> = <A as Integer>::AddCarry<B, C>;
type PushBit<A: Integer, B: Bit> = B::PushOntoInt<A>;
pub type And<A: Integer, B: Integer> = <A as Integer>::And<B>;
pub type Or<A: Integer, B: Integer> = <A as Integer>::Or<B>;
pub type Xor<A: Integer, B: Integer> = <A as Integer>::Xor<B>;
pub type Nand<A: Integer, B: Integer> = <A as Integer>::Nand<B>;
pub type Nor<A: Integer, B: Integer> = <A as Integer>::Nor<B>;
pub type Nxor<A: Integer, B: Integer> = <A as Integer>::Nxor<B>;
pub type Not<A: Integer> = <A as Integer>::Not;
pub type Shl<A: Integer, B: Unsigned> = <<B as Unsigned>::AsPeano as Peano>::Shl<A, B0>;
pub type Shr<A: Integer, B: Unsigned> = <<B as Unsigned>::AsPeano as Peano>::Shr<A>;
pub type BitGet<A: Integer, B: Unsigned> = <A as Integer>::BitGet<<B as Unsigned>::AsPeano>;
pub type BitGetP<A: Integer, B: Peano> = <A as Integer>::BitGet<B>;
pub type BitSet<A: Integer, B: Unsigned, C: Bit> =
    <A as Integer>::BitSet<<B as Unsigned>::AsPeano, C>;
pub type BitSetP<A: Integer, B: Peano, C: Bit> = <A as Integer>::BitSet<B, C>;
pub type SignumOf<A: Integer> = <<A as Integer>::CompareZero as Ordering>::ToSignum; // ::PickInt<IOnes, IZeros, consts::P1>;

pub type Sum<A: Integer, B: Integer> = <A as Integer>::Add<B>;
pub type Diff<A: Integer, B: Integer> = <A as Integer>::Add<B::Neg>;

pub type ToUnsignedUnchecked<A: Integer> = <A as Integer>::ToUnsignedUnchecked;

pub type Inc<A: Integer> = <A as Integer>::Inc;
pub type Dec<A: Integer> = <A as Integer>::Dec;
pub type Prod<A: Integer, B: Integer> = <A as Integer>::Mul<B>;
pub type Pow<A: Integer, B: Unsigned> = <A as Integer>::Pow<B>;
pub type Sq<A: Integer> = <A as Integer>::Mul<A>;
pub type Neg<A: Integer> = <A as Integer>::Neg;
pub type Abs<A: Integer> = <A as Integer>::Abs;
pub type Length<A: Integer> = <A as Integer>::Length;

pub type ProdSig<A: Signum, B: Signum> = <A as Signum>::MulSignum<B>;

pub unsafe trait ArrayStore {
    type Item;
    type Length: Unsigned;
}

unsafe impl<T> ArrayStore for [T; 0] {
    type Item = T;
    type Length = IZeros;
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct IntArrayStore<M, T, B: Bit>(M, M, B::MaybeItem<T>);

unsafe impl<M: ArrayStore, B: Bit> ArrayStore for IntArrayStore<M, M::Item, B> {
    type Item = M::Item;
    type Length = Int<M::Length, B>;
}

pub trait Unsigned: Integer {
    const TO_BOOL: bool = match Self::TO_U8 {
        0 => false,
        1 => true,
        _ => panic!("Invalid bool"),
    };
    const TO_U8: u8;
    const TO_U16: u16;
    const TO_U32: u32;
    const TO_U64: u64;
    const TO_U128: u128;
    const TO_USIZE: usize;

    type AsPeano: Peano;
    type MostSigU: Unsigned;
    type Exp<N: Integer>: Integer;
    type Array<T>: ArrayStore<Item = T, Length = Self>;
}
impl Unsigned for IZeros {
    const TO_U8: u8 = 0;
    const TO_U16: u16 = 0;
    const TO_U32: u32 = 0;
    const TO_U64: u64 = 0;
    const TO_U128: u128 = 0;
    const TO_USIZE: usize = 0;

    type AsPeano = PZ;
    type MostSigU = Self;
    type Exp<N: Integer> = consts::P1;
    type Array<T> = [T; 0];
}
impl<M: Unsigned, L: Bit> Unsigned for Int<M, L> {
    const TO_U8: u8 = 2 * M::TO_U8 + L::TO_BOOL as u8;
    const TO_U16: u16 = 2 * M::TO_U16 + L::TO_BOOL as u16;
    const TO_U32: u32 = 2 * M::TO_U32 + L::TO_BOOL as u32;
    const TO_U64: u64 = 2 * M::TO_U64 + L::TO_BOOL as u64;
    const TO_U128: u128 = 2 * M::TO_U128 + L::TO_BOOL as u128;
    const TO_USIZE: usize = 2 * M::TO_USIZE + L::TO_BOOL as usize;

    type MostSigU = M;

    type AsPeano = L::AddPeano<<M::AsPeano as Peano>::Add<M::AsPeano>>;
    // N ^ (2 bm + bl) =
    // N ^ (2 bm) * N ^ bl =
    // (N * N) ^ bm * N ^ bl =
    type Exp<N: Integer> = Prod<Pow<Prod<N, N>, M>, <L as Bit>::Pow<N>>;
    type Array<T> = IntArrayStore<M::Array<T>, T, L>;
}

pub trait NonZero: Integer {}
impl NonZero for IOnes {}
impl<M: NonZero> NonZero for Int<M, B0> {}
impl<M: Integer> NonZero for Int<M, B1> {}

pub trait Negative: Integer<CompareZero = OrdLess> + NonZero {}
pub trait Positive: Integer<CompareZero = OrdGreater> + Unsigned + NonZero {}
pub trait IsZero: Integer<CompareZero = OrdEq> + Unsigned {}
impl<T: Integer<CompareZero = OrdLess> + NonZero> Negative for T {}
impl<T: Integer<CompareZero = OrdGreater> + Unsigned + NonZero> Positive for T {}
impl<T: Integer<CompareZero = OrdEq> + Unsigned> IsZero for T {}

type Cmp<A, B> = CmpZero<Diff<A, B>>;
type CmpZero<A> = <A as Integer>::CompareZero;

pub mod cmp {
    use super::{Diff, Integer, Positive};
    use crate::{IsZero, Negative, OrdEq, OrdGreater, OrdLess, Ordering, Unsigned};

    pub trait Gt<B: Integer>: Integer {}
    impl<A: Integer, B: Integer> Gt<B> for A where Diff<A, B>: Positive {}
    pub trait Lt<B: Integer>: Integer {}
    impl<A: Integer, B: Integer> Lt<B> for A where Diff<A, B>: Negative {}
    pub trait Eq<B: Integer>: Integer {}
    impl<A: Integer, B: Integer> Eq<B> for A where Diff<A, B>: IsZero {}

    pub trait Ge<B: Integer>: Integer {}
    impl<A: Integer, B: Integer> Ge<B> for A where Diff<A, B>: Unsigned {}
    pub trait Le<B: Integer>: Integer {}
    impl<A: Integer, B: Integer> Le<B> for A where Diff<B, A>: Unsigned {}
    pub trait Ne<B: Integer>: Integer {}
    impl<A: Integer, B: Integer> Ne<B> for A where <Diff<A, B> as Integer>::CompareZero: OrdNe {}

    pub trait OrdLe: Ordering {}
    pub trait OrdGe: Ordering {}
    pub trait OrdNe: Ordering {}

    impl OrdGe for OrdGreater {}
    impl OrdNe for OrdGreater {}

    impl OrdGe for OrdEq {}
    impl OrdLe for OrdEq {}

    impl OrdLe for OrdLess {}
    impl OrdNe for OrdLess {}
}

pub trait IsPowerOfTwo: Unsigned {
    type TrailingZeros: Peano;
    type Mask: Unsigned;
}
impl<M: IsPowerOfTwo> IsPowerOfTwo for Int<M, B0> {
    type TrailingZeros = PS<M::TrailingZeros>;
    type Mask = Int<M::Mask, B0>;
}
impl<M: IsZero> IsPowerOfTwo for Int<M, B1> {
    type TrailingZeros = PZ;
    type Mask = IZeros;
}

pub trait Div<D: NonZero> {
    type Quot: Integer;
    type Rem: Unsigned;
}

pub type Quot<N, D> = <N as Div<D>>::Quot;
pub type Rem<N, D> = <N as Div<D>>::Rem;

type SigDiv<A: Signum, B: Signum> = <A as Signum>::DivSignums<B>;
type SigDivQuot<A: Signum, B: Signum, Q, R> = <SigDiv<A, B> as SignumPair>::Quot<Q, R>;
type SigDivRem<A: Signum, B: Signum, Q, R, D> = <SigDiv<A, B> as SignumPair>::Rem<Q, R, D>;

impl<N: Integer, D: NonZero> Div<D> for N {
    type Quot = SigDivQuot<
        SignumOf<N>,
        SignumOf<D>,
        <Length<Abs<N>> as div_private::DivLoopPeano>::StartQuot<Abs<N>, Abs<D>>,
        <Length<Abs<N>> as div_private::DivLoopPeano>::StartRem<Abs<N>, Abs<D>>,
    >;
    type Rem = SigDivRem<
        SignumOf<N>,
        SignumOf<D>,
        <Length<Abs<N>> as div_private::DivLoopPeano>::StartQuot<Abs<N>, Abs<D>>,
        <Length<Abs<N>> as div_private::DivLoopPeano>::StartRem<Abs<N>, Abs<D>>,
        Abs<D>,
    >;
}

mod div_private {
    use super::*;

    // Division algorithm:
    // We have N / D:
    // let Q = 0, R = 0
    // NBits = len(N)
    // for I in NBits-1..0:
    //   R <<=1
    //   R[0] = N[i]
    //   let C = R.cmp(D)
    //   if C == Equal or Greater:
    //     R -= D
    //     Q[i] = 1

    pub trait DivLoop<N, D, Q, R, I> {
        type Quot: Integer;
        type Rem: Unsigned;
    }
    pub trait DivLoopCmp<N, D, Q, R, I> {
        type Quot: Integer;
        type Rem: Unsigned;
    }

    pub trait DivLoopPeano {
        type StartQuot<N: Unsigned, D: Unsigned>: Integer;
        type StartRem<N: Unsigned, D: Unsigned>: Unsigned;

        type Quot<N: Unsigned, D: Unsigned, Q: Integer, R: Unsigned, O: Ordering>: Integer;
        type Rem<N: Unsigned, D: Unsigned, Q: Integer, R: Unsigned, O: Ordering>: Unsigned;
    }

    impl DivLoopPeano for PZ {
        // this can only happen if you are dividing zero
        type StartQuot<N: Unsigned, D: Unsigned> = consts::Z0;
        type StartRem<N: Unsigned, D: Unsigned> = consts::Z0;

        type Quot<N: Unsigned, D: Unsigned, Q: Integer, R: Unsigned, O: Ordering> =
            O::QuotZ<N, D, Q, R>;
        type Rem<N: Unsigned, D: Unsigned, Q: Integer, R: Unsigned, O: Ordering> =
            O::RemZ<N, D, Q, R>;
    }

    impl<I: Peano> DivLoopPeano for PS<I> {
        type StartQuot<N: Unsigned, D: Unsigned> =
            <() as DivLoop<N, D, consts::Z0, consts::Z0, I>>::Quot;
        type StartRem<N: Unsigned, D: Unsigned> =
            <() as DivLoop<N, D, consts::Z0, consts::Z0, I>>::Rem;

        type Quot<N: Unsigned, D: Unsigned, Q: Integer, R: Unsigned, O: Ordering> =
            O::QuotPS<N, D, Q, R, I>;
        type Rem<N: Unsigned, D: Unsigned, Q: Integer, R: Unsigned, O: Ordering> =
            O::RemPS<N, D, Q, R, I>;
    }

    pub trait DivLoopOrd {
        type QuotPS<N: Unsigned, D: Unsigned, Q: Integer, R: Unsigned, I: Peano>: Integer;
        type RemPS<N: Unsigned, D: Unsigned, Q: Integer, R: Unsigned, I: Peano>: Unsigned;

        type QuotZ<N: Unsigned, D: Unsigned, Q: Integer, R: Unsigned>: Integer;
        type RemZ<N: Unsigned, D: Unsigned, Q: Integer, R: Unsigned>: Unsigned;
    }

    impl DivLoopOrd for OrdLess {
        type QuotPS<N: Unsigned, D: Unsigned, Q: Integer, R: Unsigned, I: Peano> =
            <() as DivLoop<N, D, Q, R, I>>::Quot;
        type RemPS<N: Unsigned, D: Unsigned, Q: Integer, R: Unsigned, I: Peano> =
            <() as DivLoop<N, D, Q, R, I>>::Rem;

        type QuotZ<N: Unsigned, D: Unsigned, Q: Integer, R: Unsigned> = Q;
        type RemZ<N: Unsigned, D: Unsigned, Q: Integer, R: Unsigned> = R;
    }

    impl DivLoopOrd for OrdEq {
        type QuotPS<N: Unsigned, D: Unsigned, Q: Integer, R: Unsigned, I: Peano> =
            <() as DivLoop<N, D, BitSetP<Q, PS<I>, B1>, ToUnsignedUnchecked<Diff<R, D>>, I>>::Quot;
        type RemPS<N: Unsigned, D: Unsigned, Q: Integer, R: Unsigned, I: Peano> =
            <() as DivLoop<N, D, BitSetP<Q, PS<I>, B1>, ToUnsignedUnchecked<Diff<R, D>>, I>>::Rem;

        type QuotZ<N: Unsigned, D: Unsigned, Q: Integer, R: Unsigned> = BitSetP<Q, PZ, B1>;
        type RemZ<N: Unsigned, D: Unsigned, Q: Integer, R: Unsigned> =
            ToUnsignedUnchecked<Diff<R, D>>;
    }

    impl DivLoopOrd for OrdGreater {
        type QuotPS<N: Unsigned, D: Unsigned, Q: Integer, R: Unsigned, I: Peano> =
            <() as DivLoop<N, D, BitSetP<Q, PS<I>, B1>, ToUnsignedUnchecked<Diff<R, D>>, I>>::Quot;
        type RemPS<N: Unsigned, D: Unsigned, Q: Integer, R: Unsigned, I: Peano> =
            <() as DivLoop<N, D, BitSetP<Q, PS<I>, B1>, ToUnsignedUnchecked<Diff<R, D>>, I>>::Rem;

        type QuotZ<N: Unsigned, D: Unsigned, Q: Integer, R: Unsigned> = BitSetP<Q, PZ, B1>;
        type RemZ<N: Unsigned, D: Unsigned, Q: Integer, R: Unsigned> =
            ToUnsignedUnchecked<Diff<R, D>>;
    }

    impl<T, N: Unsigned, D: Unsigned, Q: Integer, R: Unsigned, I: Peano> DivLoop<N, D, Q, R, I> for T {
        type Quot = <() as DivLoopCmp<
            N,
            D,
            Q,
            ToUnsignedUnchecked<BitSet<Shl<R, consts::P1>, consts::Z0, BitGetP<N, I>>>,
            I,
        >>::Quot;
        type Rem = <() as DivLoopCmp<
            N,
            D,
            Q,
            ToUnsignedUnchecked<BitSet<Shl<R, consts::P1>, consts::Z0, BitGetP<N, I>>>,
            I,
        >>::Rem;
    }

    impl<T, N: Unsigned, D: Unsigned, Q: Integer, R: Unsigned, I: Peano> DivLoopCmp<N, D, Q, R, I>
        for T
    {
        type Quot = <I as DivLoopPeano>::Quot<N, D, Q, R, Cmp<R, D>>;
        type Rem = <I as DivLoopPeano>::Rem<N, D, Q, R, Cmp<R, D>>;
    }
}
