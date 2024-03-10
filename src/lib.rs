#![allow(type_alias_bounds)]
#![recursion_limit = "256"]

pub mod consts;

#[cfg(test)]
mod tests;

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
    type PickUInt<L: Unsigned, E: Unsigned, G: Unsigned>: Unsigned;
    type ToSignum: Signum;
}

pub struct OrdLess;
pub struct OrdEq;
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
    const NEW: Self;
    // If then else, if the bit is true then T is returned else F is returned
    type Ite<T: Bit, F: Bit>: Bit;
    type PickInt<T: Integer, F: Integer>: Integer;
    type PushOntoInt<I: Integer>: Integer<LeastSig = Self, MostSig = I, BitGet<PZ> = Self>;
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

pub trait Peano: Seal + Sized {
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

    type BitGet<P: Peano>: Bit;
    type BitSet<P: Peano, B: Bit>: Integer;

    type CompareZero: Ordering;
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
    type BitGet<P: Peano> = P::BitGet<M, L>;
    type BitSet<P: Peano, B: Bit> = P::BitSet<M, L, B>;

    type CompareZero = <M::CompareZero as Ordering>::Then<L::CompareZero>;
}

impl Signum for consts::N1 {
    type Negate = consts::P1;
    type MulSignum<N: Signum> = N::Negate;
}

impl Signum for consts::Z0 {
    type Negate = consts::Z0;
    type MulSignum<N: Signum> = consts::Z0;
}

impl Signum for consts::P1 {
    type Negate = consts::N1;
    type MulSignum<N: Signum> = N;
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
pub type Inc<A: Integer> = <A as Integer>::Inc;
pub type Dec<A: Integer> = <A as Integer>::Dec;
pub type Prod<A: Integer, B: Integer> = <A as Integer>::Mul<B>;
pub type Sq<A: Integer> = <A as Integer>::Mul<A>;
pub type Neg<A: Integer> = <A as Integer>::Neg;
pub type Abs<A: Integer> = <A as Integer>::Abs;
pub type Length<A: Integer> = <A as Integer>::Length;

pub type ProdSig<A: Signum, B: Signum> = <A as Signum>::MulSignum<B>;

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

impl<N: Integer, D: NonZero> Div<D> for N
where
    // CmpZero<Prod<Signum<N>, Signum<D>>>:,
    (): div_private::DivStart<Abs<N>, Abs<D>, SignumOf<N>, ProdSig<SignumOf<N>, SignumOf<D>>>,
{
    type Quot = <() as div_private::DivStart<
        Abs<N>,
        Abs<D>,
        SignumOf<N>,
        ProdSig<SignumOf<N>, SignumOf<D>>,
    >>::Quot;
    type Rem = <() as div_private::DivStart<
        Abs<N>,
        Abs<D>,
        SignumOf<N>,
        ProdSig<SignumOf<N>, SignumOf<D>>,
    >>::Rem;
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

    pub trait DivStart<N, D, NSign, Sign> {
        type Quot: Integer;
        type Rem: Unsigned;
    }

    pub trait DivStartLoop<N, D, I> {
        type Quot: Integer;
        type Rem: Unsigned;
    }

    pub trait DivLoop<N, D, Q, R, I> {
        type Quot: Integer;
        type Rem: Unsigned;
    }
    pub trait DivLoopCmp<N, D, Q, R, I> {
        type Quot: Integer;
        type Rem: Unsigned;
    }
    pub trait DivLoopIf<N, D, Q, R, I, Cmp> {
        type Quot: Integer;
        type Rem: Unsigned;
    }

    // Zero / D == Zero
    impl<T, N: Integer, D: NonZero> DivStart<N, D, IZeros, IZeros> for T {
        type Quot = IZeros;
        type Rem = IZeros;
    }

    impl<T, N: Integer, D: NonZero> DivStart<N, D, consts::P1, consts::P1> for T
    where
        // NBits = len(N)
        T: DivStartLoop<Abs<N>, Abs<D>, Length<Abs<N>>>,
    {
        type Quot = T::Quot;
        type Rem = T::Rem;
    }

    impl<T, N: Integer, D: NonZero> DivStart<N, D, consts::P1, consts::N1> for T
    where
        // NBits = len(N)
        T: DivStartLoop<Abs<N>, Abs<D>, Length<Abs<N>>>,
        Diff<D, T::Rem>: Unsigned,
    {
        type Quot = Neg<T::Quot>;
        type Rem = T::Rem;
    }

    impl<T, N: Integer, D: NonZero> DivStart<N, D, consts::N1, consts::P1> for T
    where
        // NBits = len(N)
        T: DivStartLoop<Abs<N>, Abs<D>, Length<Abs<N>>>,
        Diff<D, T::Rem>: Unsigned,
    {
        type Quot = <CmpZero<T::Rem> as Ordering>::PickInt<consts::Z0, T::Quot, Inc<T::Quot>>;
        type Rem = <CmpZero<T::Rem> as Ordering>::PickUInt<consts::Z0, T::Rem, Diff<D, T::Rem>>;
    }

    impl<T, N: Integer, D: NonZero> DivStart<N, D, consts::N1, consts::N1> for T
    where
        // NBits = len(N)
        T: DivStartLoop<Abs<N>, Abs<D>, Length<Abs<N>>>,
        Diff<D, T::Rem>: Unsigned,
    {
        type Quot = <CmpZero<T::Rem> as Ordering>::PickInt<consts::Z0, Neg<T::Quot>, Not<T::Quot>>;
        type Rem = <CmpZero<T::Rem> as Ordering>::PickUInt<consts::Z0, T::Rem, Diff<D, T::Rem>>;
    }

    impl<T, N: Integer, D, I: Peano> DivStartLoop<N, D, PS<I>> for T
    where
        // let Q = 0, R = 0
        // for I in NBits-1..0:
        T: DivLoop<N, D, consts::Z0, consts::Z0, I>,
    {
        type Quot = T::Quot;
        type Rem = T::Rem;
    }

    impl<T, N: Integer, D, Q, R: Integer, I: Peano> DivLoop<N, D, Q, R, I> for T
    where
        //   R <<=1
        //   R[0] = N[i]
        T: DivLoopCmp<N, D, Q, BitSet<Shl<R, consts::P1>, consts::Z0, BitGetP<N, I>>, I>,
    {
        type Quot = T::Quot;
        type Rem = T::Rem;
    }

    impl<T, N, D: Integer, Q, R: Integer, I> DivLoopCmp<N, D, Q, R, I> for T
    where
        //   let C = R.cmp(D)
        T: DivLoopIf<N, D, Q, R, I, Cmp<R, D>>,
    {
        type Quot = T::Quot;
        type Rem = T::Rem;
    }

    impl<T, N, D: Integer, Q: Integer, R: Integer, I: Peano, O: cmp::OrdGe>
        DivLoopIf<N, D, Q, R, PS<I>, O> for T
    where
        //     R -= D
        //     Q[i] = 1
        T: DivLoop<N, D, BitSetP<Q, PS<I>, B1>, Diff<R, D>, I>,
    {
        type Quot = T::Quot;
        type Rem = T::Rem;
    }

    impl<T, N, D, Q: Integer, R: Integer, I: Peano> DivLoopIf<N, D, Q, R, PS<I>, OrdLess> for T
    where
        T: DivLoop<N, D, Q, R, I>,
    {
        type Quot = T::Quot;
        type Rem = T::Rem;
    }

    //     R -= D
    //     Q[i] = 1
    impl<T, N, D: Integer, Q: Integer, R: Integer, O: cmp::OrdGe> DivLoopIf<N, D, Q, R, PZ, O> for T
    where
        Diff<R, D>: Unsigned,
    {
        type Quot = BitSetP<Q, PZ, B1>;
        type Rem = Diff<R, D>;
    }

    impl<T, N, D, Q: Integer, R: Unsigned> DivLoopIf<N, D, Q, R, PZ, OrdLess> for T {
        type Quot = Q;
        type Rem = R;
    }
}
