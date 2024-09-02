use crate::btf::{Btf, Fwd, InnerType, Type, TypeKind};
use anyhow::{bail, Result};
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::AddressSpace;

pub fn generate_function_signature(
    function_id: u32,
    name: &str,
    btf: &Btf,
    context: &Context,
) -> Result<String> {
    generate_functions_signature([(function_id, name)].into_iter(), btf, context)
}

pub fn generate_functions_signature<'a>(
    functions: impl Iterator<Item = (u32, &'a str)>,
    btf: &'a Btf,
    context: &'a Context,
) -> Result<String> {
    let code_gen = CodeGen::new(context, btf);
    for (function_id, name) in functions {
        let res = code_gen.llvm_type_from_parsed_type(function_id)?;
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
    }
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
    btf: &'ctx Btf,
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
    pub fn new(context: &'ctx Context, btf: &'ctx Btf) -> Self {
        let module = context.create_module("");
        Self {
            module,
            context,
            btf,
        }
    }

    fn llvm_type_from_parsed_type(&self, type_index: u32) -> Result<AnyTypeEnum<'ctx>> {
        let ty = &self.btf.types()[type_index as usize];
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
            &InnerType::Pointer(type_index) => {
                let ty = self.llvm_type_from_parsed_type(type_index)?;
                let ty: BasicTypeEnum =
                    convert_to_basic_type(ty).unwrap_or_else(|| self.context.i8_type().into());
                Ok(AnyTypeEnum::PointerType(
                    ty.ptr_type(AddressSpace::default()),
                ))
            }
            &InnerType::Array {
                elem_type_index,
                num_elements,
                ..
            } => {
                let elem_ty = self.llvm_type_from_parsed_type(elem_type_index)?;
                let elem_ty: BasicTypeEnum =
                    convert_to_basic_type(elem_ty).unwrap_or_else(|| self.context.i8_type().into());
                Ok(AnyTypeEnum::ArrayType(elem_ty.array_type(num_elements)))
            }
            InnerType::Struct { .. } => {
                let Some(name) = ty.name else {
                    bail!("Struct without name aren't supported");
                };
                let name = format!("struct.{}", name);
                Ok(self.context.get_struct_type(&name).map_or_else(
                    || AnyTypeEnum::StructType(self.context.opaque_struct_type(&name)),
                    AnyTypeEnum::StructType,
                ))
            }
            InnerType::Union { .. } => {
                let Some(name) = ty.name else {
                    bail!("Union without name aren't supported");
                };
                let name = format!("union.{}", name);
                Ok(self.context.get_struct_type(&name).map_or_else(
                    || AnyTypeEnum::StructType(self.context.opaque_struct_type(&name)),
                    AnyTypeEnum::StructType,
                ))
            }
            InnerType::Enum32 { .. } => Ok(AnyTypeEnum::IntType(self.context.i32_type())),
            InnerType::Enum64 { .. } => Ok(AnyTypeEnum::IntType(self.context.i64_type())),
            InnerType::Fwd(_) => {
                bail!("Fwd type are not allowed, since they should be resolved by now");
            }

            &InnerType::Typedef(type_index)
            | &InnerType::Volatile(type_index)
            | &InnerType::Const(type_index)
            | &InnerType::Restrict(type_index)
            | &InnerType::Function { type_index, .. } => {
                self.llvm_type_from_parsed_type(type_index)
            }
            InnerType::FunctionProto { ret, args } => {
                let arg_types: Vec<BasicMetadataTypeEnum> = args
                    .iter()
                    .map(|arg| {
                        self.llvm_type_from_parsed_type(arg.type_index)
                            .unwrap_or_else(|_| self.context.i8_type().into())
                    })
                    .map(|x| {
                        convert_to_basic_type(x)
                            .unwrap_or_else(|| self.context.i8_type().into())
                            .into()
                    })
                    .collect::<Vec<BasicMetadataTypeEnum>>();

                let ret_type = self.llvm_type_from_parsed_type(*ret)?;
                match convert_to_basic_type(ret_type) {
                    Some(ret_type) => Ok(ret_type.fn_type(&arg_types, false).into()),
                    None => Ok(self.context.void_type().fn_type(&arg_types, false).into()),
                }
            }

            _ => bail!("Unsupported type {:?}", ty),
        }
    }
}
