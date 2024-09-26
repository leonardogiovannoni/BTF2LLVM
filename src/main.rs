#![feature(const_option)]
#![warn(rust_2018_idioms)]

use btf::Btf;
use codegen::debug_sos_field;
use codegen::struct_analyze;
use codegen::struct_analyze2;
use codegen::ArenaIndex;
pub mod btf;
mod codegen;


fn main() {
    let bytes = std::fs::read("/sys/kernel/btf/vmlinux").unwrap();
    let btf = Btf::new(&bytes).unwrap();
    let now = std::time::Instant::now();
    //let res = struct_analyze(&btf).unwrap();
    let (res, mapping) = struct_analyze2(&btf).unwrap();
    println!("Time: {:?}", now.elapsed());
    let mut index = None;
    for i in 0..btf.len() {
        if let Some(name) = btf[i].name {
            if name == "file" {
                index = Some(i);
                break;
            }
        }
    }
    let index = index.unwrap() as u32;
    let Some(arena_index ) = ArenaIndex::from_u32(index, &btf) else {
        panic!("Failed to get arena index");
    };
    let arena_index = mapping[&arena_index];
    debug_sos_field(arena_index, &res).unwrap()
}
