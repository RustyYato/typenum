fn main() {
    println!("use super::*;");
    println!("pub type Z0 = IZeros;");
    println!("pub const Z0: Z0 = IZeros;");
    for i in 1..1024 {
        let div2 = i / 2;
        let mod2 = i % 2;
        if div2 == 0 {
            println!("pub type P{i} = Int<Z0, B{mod2}>;");
        } else {
            println!("pub type P{i} = Int<P{div2}, B{mod2}>;");
        }
        println!("pub type N{i} = Neg<P{i}>;");
        println!("pub const P{i}: P{i} = Integer::NEW;");
        println!("pub const N{i}: N{i} = Integer::NEW;");
    }

    for i in 10..128 {
        let i = 1u128 << i;
        let div2 = i / 2;
        let mod2 = i % 2;
        println!("pub type P{i} = Int<P{div2}, B{mod2}>;");
        println!("pub type N{i} = Neg<P{i}>;");
        println!("pub const P{i}: P{i} = Integer::NEW;");
        println!("pub const N{i}: P{i} = Integer::NEW;");
    }
}
