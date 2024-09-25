#![feature(const_option)]
#![warn(rust_2018_idioms)]

use anyhow::bail;
use anyhow::Result;
use codegen::debug_sos_field;
use codegen::ArenaIndex;
use codegen::CodeGen;
use codegen::BTF;
use cxx::{CxxString, CxxVector};
use inkwell::context::Context;
pub mod btf;
use btf::Btf;
mod codegen;


fn main() {
    let btf = &*BTF;
    let index = btf.type_index_by_name("file", btf::TypeKind::Struct).unwrap().unwrap();
    let codegen = CodeGen::new(&btf);
    let now = std::time::Instant::now();
    let res = codegen.struct_analyze().unwrap();
    println!("Time: {:?}", now.elapsed());
    //let Some(arena_index ) = ArenaIndex::from_u32(index, &btf) else {
    //    panic!("Failed to get arena index");
    //};
    //debug_sos_field(arena_index, &res).unwrap()
}
