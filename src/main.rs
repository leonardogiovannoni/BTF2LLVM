use inkwell::context::Context;
use nom::bytes::complete::take;
use nom::combinator::{map, map_res};
use nom::error::ErrorKind;
use nom::multi::{count, fold_many0};
use nom::number::Endianness;
use nom::sequence::preceded;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::ops::Not;

use nom::Finish;
use nom::{
    bytes::complete::take_until,
    number::complete::i32,
    number::complete::{u16, u32, u8},
    sequence::tuple,
    IResult,
};
mod codegen;
use codegen::generate_function_signature;

#[derive(Clone, Copy, Debug, Default)]
struct Header {
    _version: u8,
    _flags: u8,
    _hdr_len: u32,
    type_off: u32,
    type_len: u32,
    str_off: u32,
    str_len: u32,
}

#[derive(Clone, Copy, Debug, Default)]
enum TypeKind {
    #[default]
    Void = 0,
    Integer = 1,
    Pointer = 2,
    Array = 3,
    Struct = 4,
    Union = 5,
    Enum32 = 6,
    Fwd = 7,
    Typedef = 8,
    Volatile = 9,
    Const = 10,
    Restrict = 11,
    Function = 12,
    FunctionProto = 13,
    Variable = 14,
    DataSection = 15,
    Float = 16,
    DeclTag = 17,
    TypeTag = 18,
    Enum64 = 19,
}

impl TryFrom<u32> for TypeKind {
    type Error = ();

    fn try_from(value: u32) -> Result<Self, ()> {
        match value {
            x if x == TypeKind::Array as u32 => Ok(TypeKind::Array),
            x if x == TypeKind::Const as u32 => Ok(TypeKind::Const),
            x if x == TypeKind::DataSection as u32 => Ok(TypeKind::DataSection),
            x if x == TypeKind::DeclTag as u32 => Ok(TypeKind::DeclTag),
            x if x == TypeKind::Enum32 as u32 => Ok(TypeKind::Enum32),
            x if x == TypeKind::Enum64 as u32 => Ok(TypeKind::Enum64),
            x if x == TypeKind::Float as u32 => Ok(TypeKind::Float),
            x if x == TypeKind::Function as u32 => Ok(TypeKind::Function),
            x if x == TypeKind::FunctionProto as u32 => Ok(TypeKind::FunctionProto),
            x if x == TypeKind::Fwd as u32 => Ok(TypeKind::Fwd),
            x if x == TypeKind::Integer as u32 => Ok(TypeKind::Integer),
            x if x == TypeKind::Pointer as u32 => Ok(TypeKind::Pointer),
            x if x == TypeKind::Restrict as u32 => Ok(TypeKind::Restrict),
            x if x == TypeKind::Struct as u32 => Ok(TypeKind::Struct),
            x if x == TypeKind::TypeTag as u32 => Ok(TypeKind::TypeTag),
            x if x == TypeKind::Typedef as u32 => Ok(TypeKind::Typedef),
            x if x == TypeKind::Union as u32 => Ok(TypeKind::Union),
            x if x == TypeKind::Variable as u32 => Ok(TypeKind::Variable),
            x if x == TypeKind::Void as u32 => Ok(TypeKind::Void),
            x if x == TypeKind::Volatile as u32 => Ok(TypeKind::Volatile),
            _ => Err(()),
        }
    }
}

#[derive(Clone, Debug, Default)]
struct TypeInfo<'a> {
    name: Option<&'a str>,
    vlen: u16,
    kind_flag: bool,
    size: Option<u32>,
    ref_type: Option<u32>,
}

#[derive(Clone, Debug, Default)]
pub struct StructMember<'a> {
    pub name: Option<&'a str>,
    pub type_id: u32,
    pub offset: u32,
    pub bits: Option<u32>,
}

#[derive(Clone, Debug, Default)]
pub struct EnumEntry<'a> {
    pub name: Option<&'a str>,
    pub value: i64,
}

#[derive(Clone, Copy, Debug, Default)]
pub enum Fwd {
    #[default]
    Struct,
    Union,
}

#[derive(Clone, Copy, Debug, Default)]
pub enum Linkage {
    #[default]
    Static,
    Global,
}

#[derive(Clone, Debug, Default)]
pub struct FunctionParam<'a> {
    pub name: Option<&'a str>,
    pub type_id: u32,
}

#[derive(Clone, Copy, Debug, Default)]
pub struct SectionVariable {
    pub type_id: u32,
    pub offset: u32,
    pub size: u32,
}

#[derive(Clone, Debug, Default)]
struct Type<'a> {
    name: Option<&'a str>,
    ty: InnerType<'a>,
}

#[derive(Clone, Debug, Default)]
pub enum InnerType<'a> {
    Array {
        elem_type_id: usize,
        index_type_id: usize,
        num_elements: usize,
    },
    Const(usize),
    DataSection(Vec<SectionVariable>),
    DeclTag {
        type_id: usize,
        component_index: usize,
    },
    Enum32 {
        is_signed: bool,
        bytes: usize,
        entries: Vec<EnumEntry<'a>>,
    },
    Enum64 {
        is_signed: bool,
        bytes: usize,
        entries: Vec<EnumEntry<'a>>,
    },
    Float {
        bits: usize,
    },
    Function {
        linkage: Linkage,
        type_id: usize,
    },
    FunctionProto {
        ret: usize,
        args: Vec<FunctionParam<'a>>,
    },
    Fwd(Fwd),
    Integer {
        used_bits: usize,
        bits: usize,
        is_signed: bool,
        is_char: bool,
        is_bool: bool,
    },
    Pointer(usize),
    Restrict(usize),
    Struct {
        bytes: usize,
        fields: Vec<StructMember<'a>>,
    },
    TypeTag(usize),
    Typedef(usize),
    Union {
        bytes: usize,
        fields: Vec<StructMember<'a>>,
    },
    Variable {
        linkage: Linkage,
        type_id: usize,
    },
    #[default]
    Void,
    Volatile(usize),
}

fn parse_header(input: &[u8], en: Endianness) -> IResult<&[u8], Header> {
    map(
        tuple((u8, u8, u32(en), u32(en), u32(en), u32(en), u32(en))),
        |(_version, _flags, _hdr_len, type_off, type_len, str_off, str_len)| Header {
            _version,
            _flags,
            _hdr_len,
            type_off,
            type_len,
            str_off,
            str_len,
        },
    )(input)
}

fn read_str<'a>(
    prev: &'a [u8],
    strings: &'a [u8],
    offset: u32,
) -> IResult<&'a [u8], Option<&'a str>> {
    let (input, _) = take(offset)(strings)?;
    let (input, raw_str) = take_until("\0")(input)?;
    match std::str::from_utf8(raw_str) {
        Ok(s) => Ok((prev, s.is_empty().not().then_some(s))),
        Err(_) => Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        ))),
    }
}
fn parse_type_info<'a>(
    input: &'a [u8],
    strings: &'a [u8],
    en: Endianness,
) -> IResult<&'a [u8], (TypeKind, TypeInfo<'a>)> {
    let (input, (name_off, info, size_or_type)) = tuple((u32(en), u32(en), u32(en)))(input)?;
    let (input, name) = read_str(input, strings, name_off)?;
    let kind: TypeKind = ((info >> 24) & 0x1f).try_into().unwrap();
    let (size, ref_type) = match kind {
        TypeKind::Enum32
        | TypeKind::Enum64
        | TypeKind::Float
        | TypeKind::Integer
        | TypeKind::Struct
        | TypeKind::Union => (Some(size_or_type), None),

        TypeKind::Const
        | TypeKind::DeclTag
        | TypeKind::Function
        | TypeKind::FunctionProto
        | TypeKind::Pointer
        | TypeKind::Restrict
        | TypeKind::TypeTag
        | TypeKind::Typedef
        | TypeKind::Variable
        | TypeKind::Volatile => (None, Some(size_or_type)),

        _ => (None, None),
    };

    let type_info = TypeInfo {
        name,
        vlen: info as u16,
        kind_flag: (info >> 16) & 0x1 == 0x1,
        size,
        ref_type,
    };
    Ok((input, (kind, type_info)))
}
fn parse_integer<'a>(
    input: &'a [u8],
    type_info: &TypeInfo<'a>,
    en: Endianness,
) -> IResult<&'a [u8], InnerType<'a>> {
    let (input, kind_specific) = u32(en)(input)?;
    let bits = kind_specific as u8;
    let is_signed = (kind_specific >> 24) & 0x1 == 0x1;
    let is_char = (kind_specific >> 24) & 0x2 == 0x2;
    let is_bool = (kind_specific >> 24) & 0x4 == 0x4;

    Ok((
        input,
        InnerType::Integer {
            bits: type_info.size.unwrap() as usize * 8,
            used_bits: bits.into(),
            is_signed,
            is_char,
            is_bool,
        },
    ))
}

fn parse_function<'a>(input: &'a [u8], type_info: &TypeInfo) -> IResult<&'a [u8], InnerType<'a>> {
    let linkage = if type_info.vlen == 0 {
        Linkage::Static
    } else {
        Linkage::Global
    };

    Ok((
        input,
        InnerType::Function {
            linkage,
            type_id: type_info.ref_type.unwrap() as usize,
        },
    ))
}

fn parse_array(input: &[u8], en: Endianness) -> IResult<&[u8], InnerType> {
    let (input, elem_type_id) = u32(en)(input)?;
    let (input, index_type_id) = u32(en)(input)?;
    let (input, num_elements) = u32(en)(input)?;

    Ok((
        input,
        InnerType::Array {
            elem_type_id: elem_type_id as usize,
            index_type_id: index_type_id as usize,
            num_elements: num_elements as usize,
        },
    ))
}

fn parse_enum_entry32<'a>(
    input: &'a [u8],
    strings: &'a [u8],
    en: Endianness,
) -> IResult<&'a [u8], EnumEntry<'a>> {
    let (input, name_offset) = u32(en)(input)?;
    let (input, name) = read_str(input, strings, name_offset)?;
    let (input, value) = i32(en)(input)?;
    Ok((
        input,
        EnumEntry {
            name,
            value: value as i64,
        },
    ))
}

fn parse_enum_entry64<'a>(
    input: &'a [u8],
    strings: &'a [u8],
    en: Endianness,
) -> IResult<&'a [u8], EnumEntry<'a>> {
    let (input, name_offset) = u32(en)(input)?;
    let (input, name) = read_str(input, strings, name_offset)?;

    let (input, low) = i32(en)(input)?;
    let (input, high) = i32(en)(input)?;

    Ok((
        input,
        EnumEntry {
            name,
            value: low as i64 | ((high as i64) << 32),
        },
    ))
}

fn parse_enum32<'a>(
    input: &'a [u8],
    type_info: &TypeInfo<'a>,
    strings: &'a [u8],
    en: Endianness,
) -> IResult<&'a [u8], InnerType<'a>> {
    let is_signed = type_info.kind_flag;
    let num_entries = type_info.vlen as usize;
    let (input, entries) = count(|i| parse_enum_entry32(i, strings, en), num_entries)(input)?;
    let bytes = type_info.size.unwrap() as usize;
    Ok((
        input,
        InnerType::Enum32 {
            is_signed,
            bytes,
            entries,
        },
    ))
}

fn parse_enum64<'a>(
    input: &'a [u8],
    type_info: &TypeInfo<'a>,
    strings: &'a [u8],
    en: Endianness,
) -> IResult<&'a [u8], InnerType<'a>> {
    let is_signed = type_info.kind_flag;
    let num_entries = type_info.vlen as usize;
    let (input, entries) = count(|i| parse_enum_entry64(i, strings, en), num_entries)(input)?;
    let bytes = type_info.size.unwrap() as usize;
    Ok((
        input,
        InnerType::Enum64 {
            is_signed,
            bytes,
            entries,
        },
    ))
}

fn parse_struct_member<'a>(
    input: &'a [u8],
    type_info: &TypeInfo<'a>,
    strings: &'a [u8],
    en: Endianness,
) -> IResult<&'a [u8], StructMember<'a>> {
    let (input, name_off) = u32(en)(input)?;
    let (input, name) = read_str(input, strings, name_off)?;
    let (input, type_id) = u32(en)(input)?;
    let (input, offset_and_bits) = u32(en)(input)?;

    let (offset, bits) = if type_info.kind_flag {
        (offset_and_bits & 0x00FF_FFFF, Some(offset_and_bits >> 24))
    } else {
        (offset_and_bits, None)
    };

    Ok((
        input,
        StructMember {
            name,
            type_id,
            offset,
            bits,
        },
    ))
}

fn parse_struct<'a>(
    input: &'a [u8],
    type_info: &TypeInfo<'a>,
    strings: &'a [u8],
    en: Endianness,
) -> IResult<&'a [u8], InnerType<'a>> {
    let num_members = type_info.vlen;
    let (input, members) = count(
        |input| parse_struct_member(input, type_info, strings, en),
        num_members as usize,
    )(input)?;
    let bytes = type_info.size.unwrap() as usize;
    Ok((
        input,
        InnerType::Struct {
            bytes,
            fields: members,
        },
    ))
}

fn parse_union<'a>(
    input: &'a [u8],
    type_info: &TypeInfo<'a>,
    strings: &'a [u8],
    en: Endianness,
) -> IResult<&'a [u8], InnerType<'a>> {
    let num_members = type_info.vlen;
    let (input, members) = count(
        |input| parse_struct_member(input, type_info, strings, en),
        num_members as usize,
    )(input)?;
    let bytes = type_info.size.unwrap() as usize;
    Ok((
        input,
        InnerType::Union {
            bytes,
            fields: members,
        },
    ))
}

fn parse_function_param<'a>(
    input: &'a [u8],
    strings: &'a [u8],
    en: Endianness,
) -> IResult<&'a [u8], FunctionParam<'a>> {
    map(tuple((u32(en), u32(en))), move |(name_off, type_id)| {
        let (_, name) = read_str(input, strings, name_off).unwrap_or((input, None));
        FunctionParam { name, type_id }
    })(input)
}

fn parse_function_proto<'a>(
    input: &'a [u8],
    type_info: &TypeInfo<'a>,
    strings: &'a [u8],
    en: Endianness,
) -> IResult<&'a [u8], InnerType<'a>> {
    let num_params = type_info.vlen as usize;
    let ret = type_info.ref_type.unwrap() as usize;
    let (input, args) = count(|i| parse_function_param(i, strings, en), num_params)(input)?;
    Ok((input, InnerType::FunctionProto { ret, args }))
}

fn parse_variable<'a>(
    input: &'a [u8],
    type_info: &TypeInfo<'a>,
    en: Endianness,
) -> IResult<&'a [u8], InnerType<'a>> {
    let (input, linkage) = map_res(u32(en), |value| match value {
        0 => Ok(Linkage::Static),
        1 => Ok(Linkage::Global),
        _ => Err(nom::error::ErrorKind::Tag),
    })(input)?;
    let type_id = type_info.ref_type.unwrap() as usize;
    Ok((input, InnerType::Variable { type_id, linkage }))
}

fn parse_decl_tag<'a>(
    input: &'a [u8],
    type_info: &TypeInfo<'a>,
    en: Endianness,
) -> IResult<&'a [u8], InnerType<'a>> {
    let (input, component_index) = u32(en)(input)?;
    let type_id = type_info.ref_type.unwrap() as usize;
    Ok((
        input,
        InnerType::DeclTag {
            type_id,
            component_index: component_index as usize,
        },
    ))
}

fn parse_section_variable(input: &[u8], en: Endianness) -> IResult<&[u8], SectionVariable> {
    let (input, type_id) = u32(en)(input)?;
    let (input, offset) = u32(en)(input)?;
    let (input, size) = u32(en)(input)?;

    Ok((
        input,
        SectionVariable {
            type_id,
            offset,
            size,
        },
    ))
}

fn parse_data_section<'a>(
    input: &'a [u8],
    type_info: &TypeInfo<'a>,
    en: Endianness,
) -> IResult<&'a [u8], InnerType<'a>> {
    let num_vars = type_info.vlen as usize;

    let (input, vars) = count(|i| parse_section_variable(i, en), num_vars)(input)?;

    Ok((input, InnerType::DataSection(vars)))
}

fn parse_type<'a>(
    input: &'a [u8],
    kind: TypeKind,
    type_info: &TypeInfo<'a>,
    strings: &'a [u8],
    en: Endianness,
) -> IResult<&'a [u8], InnerType<'a>> {
    match kind {
        TypeKind::Array => parse_array(input, en),
        TypeKind::Const => Ok((
            input,
            InnerType::Const(type_info.ref_type.unwrap() as usize),
        )),
        TypeKind::DataSection => parse_data_section(input, type_info, en),
        TypeKind::DeclTag => parse_decl_tag(input, type_info, en),
        TypeKind::Enum32 => parse_enum32(input, type_info, strings, en),
        TypeKind::Enum64 => parse_enum64(input, type_info, strings, en),
        TypeKind::Float => Ok((
            input,
            InnerType::Float {
                bits: type_info.size.unwrap() as usize * 8,
            },
        )),
        TypeKind::Function => parse_function(input, type_info),
        TypeKind::FunctionProto => parse_function_proto(input, type_info, strings, en),
        TypeKind::Fwd => Ok((
            input,
            InnerType::Fwd(if type_info.kind_flag {
                Fwd::Union
            } else {
                Fwd::Struct
            }),
        )),
        TypeKind::Integer => parse_integer(input, type_info, en),
        TypeKind::Pointer => Ok((
            input,
            InnerType::Pointer(type_info.ref_type.unwrap() as usize),
        )),
        TypeKind::Restrict => Ok((
            input,
            InnerType::Restrict(type_info.ref_type.unwrap() as usize),
        )),
        TypeKind::Struct => parse_struct(input, type_info, strings, en),
        TypeKind::TypeTag => Ok((
            input,
            InnerType::TypeTag(type_info.ref_type.unwrap() as usize),
        )),
        TypeKind::Typedef => Ok((
            input,
            InnerType::Typedef(type_info.ref_type.unwrap() as usize),
        )),
        TypeKind::Union => parse_union(input, type_info, strings, en),
        TypeKind::Variable => parse_variable(input, type_info, en),
        TypeKind::Void => Ok((input, InnerType::Void)),
        TypeKind::Volatile => Ok((
            input,
            InnerType::Volatile(type_info.ref_type.unwrap() as usize),
        )),
    }
}

fn parse_types<'a>(
    input: &'a [u8],
    type_off: usize,
    type_len: usize,
    strings: &'a [u8],
    en: Endianness,
) -> IResult<&'a [u8], Vec<Type<'a>>> {
    let (remaining_input, types) = preceded(take(type_off), take(type_len))(input)?;
    let (_, parsed_types) = fold_many0(
        |i| {
            let (next_input, (kind, type_info)) = parse_type_info(i, strings, en)?;
            let (final_input, ty) = parse_type(next_input, kind, &type_info, strings, en)?;
            Ok((
                final_input,
                Type {
                    name: type_info.name,
                    ty,
                },
            ))
        },
        || vec![Type::default()], // Initial accumulator
        |mut acc, item| {
            acc.push(item);
            acc
        },
    )(types)?;

    Ok((remaining_input, parsed_types))
}

fn parse_magic(input: &[u8]) -> IResult<&[u8], Endianness> {
    let (input, magic) = u16(Endianness::Little)(input)?;
    match magic {
        0xeb9f => Ok((input, Endianness::Little)),
        0x9feb => Ok((input, Endianness::Big)),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        ))),
    }
}

fn parse(input: &[u8]) -> IResult<&[u8], Vec<Type>> {
    let (input, en) = parse_magic(input)?;
    let (input, header) = parse_header(input, en)?;
    let (_, strings) = preceded(take(header.str_off), take(header.str_len))(input)?;
    let (input, types) = parse_types(
        input,
        header.type_off as usize,
        header.type_len as usize,
        strings,
        en,
    )?;
    Ok((input, types))
}

#[derive(Debug)]
struct ParserError {
    error: nom::error::ErrorKind,
}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "ParserError: {:?}", self.error)
    }
}

impl Error for ParserError {}

fn get_btf_types(data: &[u8]) -> Result<Vec<Type>, ParserError> {
    parse(data)
        .finish()
        .map(|(_, types)| types)
        .map_err(|e| ParserError { error: e.code })
}

fn main() -> Result<(), Box<dyn Error>> {
    let s = std::fs::read("/sys/kernel/btf/vmlinux")?;
    let types = get_btf_types(&s)?;
    let ctx = Context::create();

    let vfs_read = types
        .iter()
        .position(|ty| ty.name == Some("vfs_read"))
        .unwrap();

    let signature = generate_function_signature(vfs_read, &types, &ctx)?;
    println!("{}", signature);

    Ok(())
}

fn main2() -> Result<(), Box<dyn Error>> {
    let s = std::fs::read("/sys/kernel/btf/vmlinux")?;
    let types = get_btf_types(&s)?;
    println!("{:?}", types);
    Ok(())
}
