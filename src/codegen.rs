use crate::btf::{AggregateMember, Btf, Fwd, InnerType, Type, TypeKind};
use anyhow::{bail, Result};
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::AddressSpace;
use std::collections::{BTreeMap, HashSet};
use std::sync::{Arc, LazyLock};

pub struct CodeGen<'ctx> {
    btf: &'ctx Btf,
}

#[derive(Clone, Debug)]
pub struct Field<'a> {
    pub variable_name: Option<&'a str>,
    pub type_index: ArenaIndex,
    pub offset: u32,
    pub bits: Option<u32>,
}

#[derive(Clone, Debug)]
pub enum SosField<'a> {
    Array {
        elem_type_index: ArenaIndex,
        num_elements: u32,
    },
    Pointer(ArenaIndex),
    Struct {
        type_name: Option<&'a str>,
        bytes: u32,
        fields: Box<[Field<'a>]>,
    },
    TypedefStruct {
        type_name: &'a str,
        referred: ArenaIndex,
    },
}

pub static BTF: LazyLock<Btf> = LazyLock::new(|| Btf::new("/sys/kernel/btf/vmlinux").unwrap());
pub fn pretty_print_sos_field<'a>(
    index: ArenaIndex,
    sos_arena: &BTreeMap<ArenaIndex, SosField<'a>>,
    visited: &mut HashSet<ArenaIndex>,
    depth: usize,
) -> Result<(), u32> {
    let indent = "    ".repeat(depth);
    if visited.contains(&index) {
        // Recursive type detected
        if let Some(sos_field) = sos_arena.get(&index) {
            println!("{}<recursive>", indent);
        } else {
            println!("{}<recursive unknown>", indent);
        }
        return Ok(());
    }

    let sos_field = sos_arena.get(&index);
    let Some(sos_field) = sos_field else {
        // Type not in sos_arena, could be base type or unknown type
        println!(
            "{}Unknown type (index {:?}) {:?}",
            indent,
            index,
            BTF.types()[index.0 as usize]
        );
        return Ok(());
    };

    visited.insert(index);

    match &sos_field {
        &SosField::Array {
            elem_type_index,
            num_elements,
        } => {
            println!("{}[{}]", indent, num_elements,);
            pretty_print_sos_field(*elem_type_index, sos_arena, visited, depth + 1)?;
        }
        &SosField::Pointer(sub_type_index) => {
            println!("{}*", indent);
            pretty_print_sos_field(*sub_type_index, sos_arena, visited, depth + 1)?;
        }
        SosField::Struct {
            bytes: _,
            fields,
            type_name,
        } => {
            println!("{}{} {{", indent, type_name.unwrap_or(""));
            for field in fields.iter() {
                let field_name = field.variable_name.unwrap_or("<unnamed field>");
                print!("{}    {}: ", indent, field_name);
                pretty_print_sos_field(field.type_index, sos_arena, visited, depth + 1)?;
            }
            println!("{}}}", indent);
        }
        SosField::TypedefStruct {
            referred: sub_type_index,
            type_name,
        } => {
            let sub_sos_field = sos_arena.get(&sub_type_index).unwrap();
            let fields = match &sub_sos_field {
                SosField::Struct { fields, .. } => fields,
                _ => unreachable!(),
            };
            println!("{}{} {{", indent, type_name);
            for field in fields.iter() {
                let field_name = field.variable_name.unwrap_or("<unnamed field>");
                print!("{}    {}: ", indent, field_name);
                pretty_print_sos_field(field.type_index, sos_arena, visited, depth + 1)?;
            }
            println!("{}}}", indent);
        }
    }
    Ok(())
}

pub fn debug_sos_field<'a>(
    index: ArenaIndex,
    sos_arena: &BTreeMap<ArenaIndex, SosField<'a>>,
) -> Result<(), u32> {
    pretty_print_sos_field(index, sos_arena, &mut HashSet::new(), 0)
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Copy)]
pub struct ArenaIndex(u32);

impl ArenaIndex {
    pub fn from_u32(index: u32, btf: &Btf) -> Option<Self> {
        let index = strip(btf, index);
        if !eventually_resolves_to_named_struct(btf, index) {
            return None;
        }
        Some(Self(index))
    }

    pub fn from_u32_only_strip(index: u32, btf: &Btf) -> Self {
        let index = strip(btf, index);
        Self(index)
    }
}

fn eventually_resolves_to_named_struct(btf: &Btf, mut type_index: u32) -> bool {
    loop {
        let ty = &btf.types()[type_index as usize];
        match &ty.ty {
            InnerType::Struct { .. } => {
                return ty.name.is_some();
            }
            InnerType::Typedef(sub_type_index) => {
                let sub_ty = &btf.types()[*sub_type_index as usize];
                if let InnerType::Struct { .. } = &sub_ty.ty {
                    if sub_ty.name.is_none() {
                        return true;
                    }
                }
                type_index = *sub_type_index;
            }
            InnerType::Pointer(sub_type_index)
            | InnerType::Volatile(sub_type_index)
            | InnerType::Const(sub_type_index)
            | InnerType::Restrict(sub_type_index) => {
                type_index = *sub_type_index;
            }
            InnerType::Array {
                elem_type_index, ..
            } => {
                type_index = *elem_type_index;
            }
            _ => {
                return false;
            }
        }
    }
}

fn strip(btf: &Btf, ty: u32) -> u32 {
    let mut scan = ty;
    loop {
        match &btf.types()[scan as usize].ty {
            InnerType::Volatile(sub_type_index)
            | InnerType::Const(sub_type_index)
            | InnerType::Restrict(sub_type_index) => {
                scan = *sub_type_index;
            }
            InnerType::Typedef(sub_type_index) => {
                let sub_ty = &btf.types()[*sub_type_index as usize];
                if let InnerType::Struct { .. } = &sub_ty.ty {
                    if sub_ty.name.is_none() {
                        break scan;
                    }
                }
                scan = *sub_type_index;
            }
            _ => break scan,
        }
    }
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(btf: &'ctx Btf) -> Self {
        Self { btf }
    }

    pub fn struct_analyze(&self) -> Result<BTreeMap<ArenaIndex, SosField<'ctx>>> {
        let mut result_map = BTreeMap::new();

        for type_index in 0..self.btf.types().len() {
            let Some(type_index) = ArenaIndex::from_u32(type_index as u32, self.btf) else {
                continue;
            };
            if result_map.contains_key(&type_index) {
                continue;
            }
            let mut stack = vec![type_index];
            let mut visited = BTreeMap::new();

            while let Some(type_index) = stack.pop() {
                if !visited.insert(type_index, ()).is_none() {
                    continue;
                }
                let Type { name, ty } = &self.btf.types()[type_index.0 as usize];
                match ty {
                    InnerType::Pointer(sub_type_index) => {
                        if name.is_some() {
                            bail!("Pointer type should not have a name");
                        }
                        let Some(sub_type_index) = ArenaIndex::from_u32(*sub_type_index, self.btf)
                        else {
                            bail!("Pointer type does not resolve to a named struct");
                        };
                        let ptr_type = SosField::Pointer(sub_type_index);
                        result_map.insert(type_index, ptr_type);
                        stack.push(sub_type_index);
                    }
                    InnerType::Array {
                        elem_type_index,
                        num_elements,
                        ..
                    } => {
                        if name.is_some() {
                            bail!("Array type should not have a name");
                        }
                        let Some(elem_type_index) =
                            ArenaIndex::from_u32(*elem_type_index, self.btf)
                        else {
                            bail!("Array element type does not resolve to a named struct");
                        };

                        result_map.insert(
                            type_index,
                            SosField::Array {
                                elem_type_index,
                                num_elements: *num_elements,
                            },
                        );
                        stack.push(elem_type_index);
                    }
                    InnerType::Struct { bytes, fields } => {
                        let Some(name) = *name else {
                            continue;
                        };

                        let mut v = Vec::new();
                        for AggregateMember {
                            name,
                            type_index,
                            offset,
                            bits,
                        } in fields
                        {
                            let Some(type_index) = ArenaIndex::from_u32(*type_index, self.btf)
                            else {
                                continue;
                            };

                            v.push(Field {
                                variable_name: *name,
                                type_index,
                                offset: *offset,
                                bits: *bits,
                            });
                            stack.push(type_index);
                        }

                        result_map.insert(
                            type_index,
                            SosField::Struct {
                                bytes: *bytes,
                                type_name: Some(name),
                                fields: v.into_boxed_slice(),
                            },
                        );
                    }
                    InnerType::Typedef(sub_type_index) => {
                        let sub_type_index =
                            ArenaIndex::from_u32_only_strip(*sub_type_index, self.btf);
                        let Some(name) = *name else {
                            bail!("Typedef type does not have a name");
                        };
                        let sub_ty = &self.btf.types()[sub_type_index.0 as usize];
                        if let InnerType::Struct { bytes, fields } = &sub_ty.ty {
                            if sub_ty.name.is_some() {
                                stack.push(sub_type_index);
                                continue;
                            }

                            let mut v = Vec::new();
                            for AggregateMember {
                                name,
                                type_index,
                                offset,
                                bits,
                            } in fields
                            {
                                let Some(type_index) = ArenaIndex::from_u32(*type_index, self.btf)
                                else {
                                    continue;
                                };
                                v.push(Field {
                                    variable_name: *name,
                                    type_index,
                                    offset: *offset,
                                    bits: *bits,
                                });
                                stack.push(type_index);
                            }
                            result_map.insert(
                                type_index,
                                SosField::TypedefStruct {
                                    type_name: name,
                                    referred: sub_type_index,
                                },
                            );
                            result_map.insert(
                                sub_type_index,
                                SosField::Struct {
                                    type_name: None,
                                    bytes: *bytes,
                                    fields: v.into_boxed_slice(),
                                },
                            );
                        } else {
                            let Some(_) = ArenaIndex::from_u32(sub_type_index.0, self.btf) else {
                                bail!("Typedef type does not resolve to a named struct");
                            };
                            stack.push(sub_type_index);
                        }
                    }
                    _ => {}
                }
            }
        }
        Ok(result_map)
    }
}
