#![allow(type_alias_bounds)]
#![recursion_limit = "256"]

pub mod consts;

pub struct IZeros;
pub struct IOnes;
pub struct B0;
pub struct B1;
pub struct Int<M, L> {
    _most_significant_bits: M,
    _least_significant_bit: L,
}

pub struct PZ;
pub struct PS<P>(P);

use seal::Seal;
mod seal {
    pub trait Seal {}
}

pub trait Ordering: Seal {
    type Then<O: Ordering>: Ordering;
    type Reverse: Ordering<Reverse = Self>;
    type PickInt<L: Integer, E: Integer, G: Integer>: Integer;
}

pub struct OrdLess;
pub struct OrdEq;
pub struct OrdGreater;

impl Seal for OrdLess {}
impl Ordering for OrdLess {
    type Then<O: Ordering> = Self;
    type Reverse = OrdGreater;
    type PickInt<L: Integer, E: Integer, G: Integer> = L;
}
impl Seal for OrdEq {}
impl Ordering for OrdEq {
    type Then<O: Ordering> = O;
    type Reverse = Self;
    type PickInt<L: Integer, E: Integer, G: Integer> = E;
}
impl Seal for OrdGreater {}
impl Ordering for OrdGreater {
    type Then<O: Ordering> = Self;
    type Reverse = OrdLess;
    type PickInt<L: Integer, E: Integer, G: Integer> = G;
}

pub trait Bit: Seal {
    const NEW: Self;
    // If then else, if the bit is true then T is returned else F is returned
    type Ite<T: Bit, F: Bit>: Bit;
    type PickInt<T: Integer, F: Integer>: Integer;
    type PushOntoInt<I: Integer>: Integer<LeastSig = Self, MostSig = I>;
    type MaybeInc<A: Integer>: Integer;
    type MaybeDec<A: Integer>: Integer;
    type Mul<A: Integer>: Integer;
    type AddPeano<P: Peano>: Peano;

    type Not: Bit<Not = Self>;

    type CompareZero: Ordering;
}

impl Seal for B0 {}
impl Bit for B0 {
    const NEW: Self = Self;
    type Ite<T: Bit, F: Bit> = F;
    type PickInt<T: Integer, F: Integer> = F;
    type PushOntoInt<I: Integer> = I::PushLeastZero;
    type MaybeInc<A: Integer> = A;
    type MaybeDec<A: Integer> = A::Dec;
    type Mul<A: Integer> = IZeros;
    type AddPeano<P: Peano> = P;

    type Not = B1;
    type CompareZero = OrdEq;
}

impl Seal for B1 {}
impl Bit for B1 {
    const NEW: Self = Self;
    type Ite<T: Bit, F: Bit> = T;
    type PickInt<T: Integer, F: Integer> = T;
    type PushOntoInt<I: Integer> = I::PushLeastOne;
    type MaybeInc<A: Integer> = A::Inc;
    type MaybeDec<A: Integer> = A;
    type Mul<A: Integer> = A;
    type AddPeano<P: Peano> = PS<P>;

    type Not = B0;
    type CompareZero = OrdGreater;
}

pub type BitAnd<A: Bit, B: Bit> = <A as Bit>::Ite<B, B0>;
pub type BitOr<A: Bit, B: Bit> = <A as Bit>::Ite<B1, B>;
pub type BitNot<A: Bit> = <A as Bit>::Not;
pub type BitXor<A: Bit, B: Bit> = <A as Bit>::Ite<BitNot<B>, B>;
pub type BitNand<A: Bit, B: Bit> = BitNot<BitAnd<A, B>>;
pub type BitNor<A: Bit, B: Bit> = BitNot<BitOr<A, B>>;
pub type BitNxor<A: Bit, B: Bit> = BitNot<BitXor<A, B>>;
pub type BitProd<A: Bit, B: Integer> = <A as Bit>::Mul<B>;

pub trait Peano: Seal {
    type Add<B: Peano>: Peano;
    type Mul<B: Peano>: Peano;
    type Div2: Peano;
    type IncDiv2: Peano;
    type Mod2: Bit;
    type AsUnsigned: Unsigned;

    type Shl<A: Integer, Fill: Bit>: Integer;
    type Shr<A: Integer>: Integer;
}
impl Seal for PZ {}
impl Peano for PZ {
    type Add<B: Peano> = B;
    type Mul<B: Peano> = PZ;
    type Div2 = PZ;
    type IncDiv2 = PZ;
    type Mod2 = B0;
    type AsUnsigned = IZeros;

    type Shl<A: Integer, Fill: Bit> = A;
    type Shr<A: Integer> = A;
}
impl<P: Peano> Seal for PS<P> {}
impl<P: Peano> Peano for PS<P> {
    type Add<B: Peano> = PS<P::Add<B>>;
    type Mul<B: Peano> = B::Add<P::Mul<B>>;
    type Div2 = P::IncDiv2;
    type IncDiv2 = PS<P::Div2>;
    type Mod2 = BitNot<P::Mod2>;
    type AsUnsigned = Int<<Self::Div2 as Peano>::AsUnsigned, Self::Mod2>;

    type Shl<A: Integer, Fill: Bit> = PushBit<P::Shl<A, Fill>, Fill>;
    type Shr<A: Integer> = P::Shr<A::ShrOne>;
}

pub trait Integer: Seal {
    const NEW: Self;
    type LeastSig: Bit;
    type MostSig: Integer;
    type PushLeastZero: Integer<LeastSig = B0, MostSig = Self>;
    type PushLeastOne: Integer<LeastSig = B1, MostSig = Self>;

    type Inc: Integer<LeastSig = BitNot<Self::LeastSig>>;
    type Dec: Integer<LeastSig = BitNot<Self::LeastSig>>;
    type Add<B: Integer>: Integer;
    type AddCarry<B: Integer, C: Bit>: Integer;
    type Mul<B: Integer>: Integer;
    type Neg: Integer;
    type Abs: Integer;
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
    >;
    type ShrOne: Integer<LeastSig = <Self::MostSig as Integer>::LeastSig>;

    type CompareZero: Ordering;
}

impl Seal for IZeros {}
impl Integer for IZeros {
    const NEW: Self = Self;
    type LeastSig = B0;
    type MostSig = IZeros;

    type PushLeastZero = IZeros;
    type PushLeastOne = Int<IZeros, B1>;

    type Inc = Int<Self, B1>;
    type Dec = IOnes;
    type Add<B: Integer> = B;
    type AddCarry<B: Integer, C: Bit> = C::MaybeInc<B>;
    type Mul<B: Integer> = IZeros;
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

    type CompareZero = OrdEq;
}

impl Seal for IOnes {}
impl Integer for IOnes {
    const NEW: Self = Self;
    type LeastSig = B1;
    type MostSig = IOnes;

    type PushLeastZero = Int<IOnes, B0>;
    type PushLeastOne = IOnes;

    type Inc = IZeros;
    type Dec = Int<Self, B0>;
    type Add<B: Integer> = B::Dec;
    type AddCarry<B: Integer, C: Bit> = C::MaybeDec<B>;
    type Mul<B: Integer> = B::Neg;
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

    type CompareZero = OrdLess;
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
    type Neg = <Self::Not as Integer>::Inc;
    type Abs = <Self::CompareZero as Ordering>::PickInt<Self::Neg, IZeros, Self>;

    type And<B: Integer> = PushBit<And<M, B::MostSig>, BitAnd<L, B::LeastSig>>;
    type Or<B: Integer> = PushBit<Or<M, B::MostSig>, BitOr<L, B::LeastSig>>;
    type Xor<B: Integer> = PushBit<Xor<M, B::MostSig>, BitXor<L, B::LeastSig>>;
    type Nand<B: Integer> = PushBit<Nand<M, B::MostSig>, BitNand<L, B::LeastSig>>;
    type Nor<B: Integer> = PushBit<Nor<M, B::MostSig>, BitNor<L, B::LeastSig>>;
    type Nxor<B: Integer> = PushBit<Nxor<M, B::MostSig>, BitNxor<L, B::LeastSig>>;
    type Not = Int<M::Not, BitNot<L>>;
    type ShrOne = M;

    type CompareZero = <M::CompareZero as Ordering>::Then<L::CompareZero>;
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

pub type Sum<A: Integer, B: Integer> = <A as Integer>::Add<B>;
pub type Diff<A: Integer, B: Integer> = <A as Integer>::Add<B::Neg>;
pub type Prod<A: Integer, B: Integer> = <A as Integer>::Mul<B>;
pub type Sq<A: Integer> = <A as Integer>::Mul<A>;
pub type Neg<A: Integer> = <A as Integer>::Neg;
pub type Abs<A: Integer> = <A as Integer>::Abs;

pub trait Unsigned: Integer {
    type AsPeano: Peano;
}
impl Unsigned for IZeros {
    type AsPeano = PZ;
}
impl<M: Unsigned, L: Bit> Unsigned for Int<M, L> {
    type AsPeano = L::AddPeano<<M::AsPeano as Peano>::Add<M::AsPeano>>;
}

pub trait NonZero: Integer {}
impl NonZero for IOnes {}
impl<M: NonZero> NonZero for Int<M, B0> {}
impl<M: Integer> NonZero for Int<M, B1> {}

pub trait Negative: Integer<CompareZero = OrdLess> {}
pub trait Positive: Integer<CompareZero = OrdGreater> + Unsigned {}
pub trait IsZero: Integer<CompareZero = OrdEq> + Unsigned {}
impl<T: Integer<CompareZero = OrdLess>> Negative for T {}
impl<T: Integer<CompareZero = OrdGreater> + Unsigned> Positive for T {}
impl<T: Integer<CompareZero = OrdEq> + Unsigned> IsZero for T {}

pub mod cmp {
    use super::{Diff, Integer, Positive};
    use crate::{IsZero, Negative, OrdGreater, OrdLess, Ordering, Unsigned};

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
    impl<A: Integer, B: Integer> Ne<B> for A where <Diff<A, B> as Integer>::CompareZero: NotEqual {}

    pub trait NotEqual: Ordering {}
    impl NotEqual for OrdLess {}
    impl NotEqual for OrdGreater {}
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
