#![feature(const_option)]

use anyhow::bail;
use anyhow::Result;
use codegen::generate_functions_signature;
use cxx::{CxxString, CxxVector};
use inkwell::context::Context;
pub mod btf;
use btf::Btf;
mod codegen;
use codegen::generate_function_signature;

pub fn get_signatures_vector_impl<'a>(
    functions: impl Iterator<Item = &'a str>,
) -> Result<Vec<String>> {
    let btf = Btf::new("/sys/kernel/btf/vmlinux")?;
    let mut res = Vec::with_capacity(functions.size_hint().0);
    for (index, name) in functions.enumerate() {
        if name.is_empty() {
            bail!("function at index {} is not valid", index);
        }
        let Some(fun_index) = btf.type_index_by_name(name, btf::TypeKind::Function)? else {
            bail!("function {} not found", name);
        };
        let ctx = Context::create();
        let signature = generate_function_signature(fun_index, name, &btf, &ctx);
        res.push(signature?);
    }
    Ok(res)
}

pub fn get_signatures_string_impl<'a>(fun_names: impl Iterator<Item = &'a str>) -> Result<String> {
    let btf = Btf::new("/sys/kernel/btf/vmlinux")?;
    let mut functions = Vec::with_capacity(fun_names.size_hint().0);
    for (index, name) in fun_names.enumerate() {
        if name.is_empty() {
            bail!("function at index {} is not valid", index);
        }
        let Some(fun_index) = btf.type_index_by_name(name, btf::TypeKind::Function)? else {
            bail!("function {} not found", name);
        };
        functions.push((fun_index, name));
    }
    let ctx = Context::create();
    generate_functions_signature(functions.into_iter(), &btf, &ctx)
}

fn get_signatures_vector(functions: &CxxVector<CxxString>) -> Result<Vec<String>> {
    let iter = functions.iter().map(|x| x.to_str().unwrap_or_default());
    get_signatures_vector_impl(iter)
}

fn get_signatures_string(functions: &CxxVector<CxxString>) -> Result<String> {
    let iter = functions.iter().map(|x| x.to_str().unwrap_or_default());
    get_signatures_string_impl(iter)
}

#[cxx::bridge(namespace = btf2llvm)]
pub mod ffi {
    extern "Rust" {
        pub fn get_signatures_vector(functions: &CxxVector<CxxString>) -> Result<Vec<String>>;
        pub fn get_signatures_string(functions: &CxxVector<CxxString>) -> Result<String>;
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test1() {
        //let btf = Btf::new("/sys/kernel/btf/vmlinux").unwrap();
        //println!("{:#?}", btf.0.names_lookup());
        //println!("{:#?}", btf.names().collect::<Vec<_>>());
        //println!("{}", size_of::<SmallVec<[(u32, TypeKind); 1]>>());
        //let tmp = get_signatures_impl(vec!["vfs_read"].into_iter()).unwrap();
        //let res = dump_attachable_functions().unwrap();
        let res = get_signatures_string_impl(vec!["vfs_read", "vfs_write"].into_iter()).unwrap();

        println!("{}", res);
    }
}
