

use crate::btf::{Type, InnerType};
use anyhow::{bail, Result};
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::AddressSpace;

pub fn generate_function_signature(
    function_id: u32,
    name: &str,
    types: &[Type],
    context: &Context,
) -> Result<String> {
    let code_gen = CodeGen::new(context);
    let res = code_gen.llvm_type_from_parsed_type(function_id, types)?;
    let AnyTypeEnum::FunctionType(function_type) = res else {
        bail!("Expected function type")
    };
    let function = code_gen.module.add_function(name, function_type, None);
    let block = context.append_basic_block(function, "");
    let builder = context.create_builder();
    builder.position_at_end(block);
    builder
        .build_unreachable()
        .map_err(|e| anyhow::anyhow!(e))?;
    code_gen
        .module
        .verify()
        .map_err(|e| anyhow::anyhow!(e.to_string()))?;
    let res = code_gen
        .module
        .print_to_string()
        .to_string_lossy()
        .trim()
        .to_string();
    Ok(res)
}

pub struct CodeGen<'ctx> {
    module: Module<'ctx>,
    context: &'ctx Context,
}
fn convert_to_basic_type(ty: AnyTypeEnum<'_>) -> Option<BasicTypeEnum<'_>> {
    match ty {
        AnyTypeEnum::IntType(int_type) => Some(BasicTypeEnum::IntType(int_type)),
        AnyTypeEnum::FloatType(float_type) => Some(BasicTypeEnum::FloatType(float_type)),
        AnyTypeEnum::PointerType(pointer_type) => Some(BasicTypeEnum::PointerType(pointer_type)),
        AnyTypeEnum::StructType(struct_type) => Some(BasicTypeEnum::StructType(struct_type)),
        AnyTypeEnum::ArrayType(array_type) => Some(BasicTypeEnum::ArrayType(array_type)),
        AnyTypeEnum::VectorType(vector_type) => Some(BasicTypeEnum::VectorType(vector_type)),
        AnyTypeEnum::FunctionType(function_type) => Some(BasicTypeEnum::PointerType(
            function_type.ptr_type(AddressSpace::default()),
        )),
        AnyTypeEnum::VoidType(_void_type) => None,
    }
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let module = context.create_module("");
        Self { module, context }
    }

    fn llvm_type_from_parsed_type(
        &self,
        type_id: u32,
        types: &[Type],
    ) -> Result<AnyTypeEnum<'ctx>> {
        let ty = &types[type_id as usize];
        match &ty.ty {
            InnerType::Void => Ok(self.context.void_type().into()),
            &InnerType::Integer {
                // used_bits,
                bits,
                //is_signed,
                ..
            } => Ok(AnyTypeEnum::IntType(
                self.context.custom_width_int_type(bits),
            )),
            &InnerType::Pointer(type_id) => {
                let ty = self.llvm_type_from_parsed_type(type_id, types)?;
                let ty: BasicTypeEnum =
                    convert_to_basic_type(ty).unwrap_or_else(|| self.context.i8_type().into());
                Ok(AnyTypeEnum::PointerType(
                    ty.ptr_type(AddressSpace::default()),
                ))
            }
            &InnerType::Array {
                elem_type_id,
                num_elements,
                ..
            } => {
                let elem_ty = self.llvm_type_from_parsed_type(elem_type_id, types)?;
                let elem_ty: BasicTypeEnum =
                    convert_to_basic_type(elem_ty).unwrap_or_else(|| self.context.i8_type().into());
                Ok(AnyTypeEnum::ArrayType(
                    elem_ty.array_type(num_elements),
                ))
            }
            InnerType::Struct { .. } => {
                let Some(name) = ty.name else {
                    bail!("Struct without name aren't supported");
                };
                let name = format!("struct.{}", name);
                Ok(AnyTypeEnum::StructType(
                    self.context.opaque_struct_type(&name),
                ))
            }
            InnerType::Union { .. } => {
                let Some(name) = ty.name else {
                    bail!("Union without name aren't supported");
                };
                let name = format!("union.{}", name);
                Ok(AnyTypeEnum::StructType(
                    self.context.opaque_struct_type(&name),
                ))
            }
            InnerType::Enum32 { .. } => {
                bail!("Enum32 not supported")
            }
            InnerType::Enum64 { .. } => {
                bail!("Enum64 not supported")
            }
            InnerType::Fwd(_fwd) => {
                bail!("Fwd not supported")
            }
            &InnerType::Typedef(type_id)
            | &InnerType::Volatile(type_id)
            | &InnerType::Const(type_id)
            | &InnerType::Restrict(type_id)
            | &InnerType::Function { type_id, .. } => {
                self.llvm_type_from_parsed_type(type_id, types)
            }
            InnerType::FunctionProto { ret, args } => {
                let arg_types: Vec<BasicMetadataTypeEnum> = args
                    .iter()
                    .map(|arg| {
                        self.llvm_type_from_parsed_type(arg.type_id, types)
                            .unwrap_or_else(|_| self.context.i8_type().into())
                    })
                    .map(|x| {
                        convert_to_basic_type(x)
                            .unwrap_or_else(|| self.context.i8_type().into())
                            .into()
                    })
                    .collect::<Vec<BasicMetadataTypeEnum>>();

                let ret_type = self.llvm_type_from_parsed_type(*ret, types)?;
                match convert_to_basic_type(ret_type) {
                    Some(ret_type) => Ok(ret_type.fn_type(&arg_types, false).into()),
                    None => Ok(self.context.void_type().fn_type(&arg_types, false).into()),
                }
            }

            _ => bail!("Unsupported type {:?}", ty),
        }
    }
}
