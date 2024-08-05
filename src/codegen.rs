use std::error::Error;

use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::TargetTriple;
use inkwell::types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::AddressSpace;

use crate::{InnerType, Type};

pub fn generate_function_signature(
    function_id: usize,
    types: &Vec<Type>,
    context: &Context,
) -> Result<String, Box<dyn Error>> {
    let binary = CodeGen::new(context);
    let res = binary.llvm_type_from_parsed_type(function_id, types)?;
    let AnyTypeEnum::FunctionType(function_type) = res else {
        return Err("Expected function type".into());
    };
    binary.module.add_function("main", function_type, None);
    Ok(res.to_string())
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
        _ => None,
    }
}
impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let triple = TargetTriple::create("x86_64-unknown-linux-gnu");
        let module = context.create_module("dummy");
        module.set_triple(&triple);
        Self { module, context }
    }

    fn llvm_type_from_parsed_type(
        &self,
        type_id: usize,
        types: &Vec<Type>,
    ) -> Result<AnyTypeEnum<'ctx>, Box<dyn Error>> {
        let ty = &types[type_id];
        match &ty.ty {
            // Type::Void => Ok(context.void_type().into()),
            &InnerType::Integer {
                // used_bits,
                bits,
                //is_signed,
                ..
            } => Ok(AnyTypeEnum::IntType(
                self.context.custom_width_int_type(bits as u32),
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
                    elem_ty.array_type(num_elements as u32),
                ))
            }
            InnerType::Struct { .. } => {
                let Some(name) = ty.name else {
                    return Err("Struct without name aren't supported".into());
                };
                let name = format!("struct.{}", name);
                Ok(AnyTypeEnum::StructType(
                    self.context.opaque_struct_type(&name),
                ))
            }
            InnerType::Union { .. } => {
                let Some(name) = ty.name else {
                    return Err("Union without name aren't supported".into());
                };
                let name = format!("union.{}", name);
                Ok(AnyTypeEnum::StructType(
                    self.context.opaque_struct_type(&name),
                ))
            }
            InnerType::Enum32 { .. } => {
                todo!()
            }
            InnerType::Enum64 { .. } => {
                todo!()
            }
            InnerType::Fwd(_fwd) => {
                todo!()
            }
            &InnerType::Typedef(type_id) => self.llvm_type_from_parsed_type(type_id, types),
            &InnerType::Volatile(type_id) => self.llvm_type_from_parsed_type(type_id, types),
            &InnerType::Const(type_id) => self.llvm_type_from_parsed_type(type_id, types),
            &InnerType::Restrict(type_id) => self.llvm_type_from_parsed_type(type_id, types),
            &InnerType::Function { type_id, .. } => self.llvm_type_from_parsed_type(type_id, types),
            InnerType::FunctionProto { ret, args } => {
                let ret_type = self.llvm_type_from_parsed_type(*ret, types)?;
                let ret_type: BasicTypeEnum = convert_to_basic_type(ret_type).unwrap();
                let arg_types: Vec<BasicMetadataTypeEnum> = args
                    .iter()
                    .map(|arg| {
                        self.llvm_type_from_parsed_type(arg.type_id as usize, types)
                            .unwrap_or_else(|_| self.context.i8_type().into())
                    })
                    .map(|x| convert_to_basic_type(x).unwrap().into())
                    .collect::<Vec<BasicMetadataTypeEnum>>();
                Ok(ret_type.fn_type(&arg_types, false).into())
            }

            _ => Err("Unsupported type conversion".into()),
        }
    }
}
