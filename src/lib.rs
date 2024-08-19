#![feature(const_option)]

use anyhow::bail;
use anyhow::Result;
use cxx::{CxxString, CxxVector};
use inkwell::context::Context;
pub mod btf;
use btf::Btf;
mod codegen;
use codegen::generate_function_signature;

pub fn get_signatures_impl<'a>(functions: impl Iterator<Item = &'a str>) -> Result<Vec<String>> {
    let btf = Btf::new("/sys/kernel/btf/vmlinux")?;
    let mut res = Vec::with_capacity(functions.size_hint().0);
    let ctx = Context::create();
    for (index, name) in functions.enumerate() {
        if name.is_empty() {
            bail!("function at index {} is not valid", index);
        }
        let Some(fun_index) = btf.type_index_by_name(name, btf::TypeKind::Function)? else {
            bail!("function {} not found", name);
        };
        let signature = generate_function_signature(fun_index, name, btf.types(), &ctx);
        res.push(signature?);
    }
    Ok(res)
}

fn get_signatures(functions: &CxxVector<CxxString>) -> Result<Vec<String>> {
    let iter = functions.iter().map(|x| x.to_str().unwrap_or_default());
    get_signatures_impl(iter)
}

/*
fn get_signatures(functions: &CxxVector<CxxString>) -> Vec<String> {
    let iter = functions.iter().map(|x| match x.to_str() {
        Ok(s) => s,
        Err(_) => "",
    });
    match get_signatures_impl(iter) {
        Ok(r) => r,
        Err(e) => {
            let error = e.to_string();
            let mut res = vec![String::new(); functions.len()];
            res.push(error);
            res
        }
    }
} */

#[cxx::bridge(namespace = btf2llvm)]
pub mod ffi {
    extern "Rust" {
        pub fn get_signatures(functions: &CxxVector<CxxString>) -> Result<Vec<String>>;
    }
}

#[cfg(test)]
mod tests {
    use btf::{InnerType, TypeKind};
    use smallvec::SmallVec;

    use super::*;

    #[test]
    fn test1() {
        println!("{}", size_of::<SmallVec<[(u32, TypeKind); 1]>>());
        let tmp = get_signatures_impl(vec!["vfs_read"].into_iter()).unwrap();
        println!("{}", tmp[0]);
    }
}
