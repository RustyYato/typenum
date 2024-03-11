use super::consts::*;
use super::*;

const _: () = {
    let _: Quot<P30, P5> = P6;
    let _: Rem<P30, P5> = Z0;

    let _: Quot<P32, P5> = P6;
    let _: Rem<P32, P5> = P2;

    let _: Quot<N30, P5> = N6;
    let _: Rem<N30, P5> = Z0;

    let _: Quot<N32, P5> = N7;
    let _: Rem<N32, P5> = P3;

    let _: Quot<P30, N5> = N6;
    let _: Rem<P30, N5> = Z0;

    let _: Quot<P32, N5> = N6;
    let _: Rem<P32, N5> = P2;

    let _: Quot<N30, N5> = P6;
    let _: Rem<N30, N5> = Z0;

    let _: Quot<N32, N5> = P7;
    let _: Rem<N32, N5> = P3;

    let _: Quot<Z0, P5> = Z0;
    let _: Rem<Z0, P5> = Z0;

    let _: Quot<Z0, N5> = Z0;
    let _: Rem<Z0, N5> = Z0;

    let _: Pow<N1, P2> = P1;
    let _: Pow<N1, P3> = N1;

    let _: Pow<N2, P3> = N8;
    let _: Pow<N4, P3> = N64;
    let _: Pow<N4, P2> = P16;
    let _: Pow<P4, P2> = P16;
    let _: Pow<P2, P3> = P8;
};
