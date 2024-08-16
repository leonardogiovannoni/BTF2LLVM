
fn main() {
    cxx_build::bridge("src/lib.rs")
        .file("src/lib.cc")
        .static_flag(true)
        .std("c++23")
        .compile("libbtf2llvm");

    println!("cargo:rerun-if-changed=src/lib.rs");
    println!("cargo:rerun-if-changed=src/lib.cc");
}
