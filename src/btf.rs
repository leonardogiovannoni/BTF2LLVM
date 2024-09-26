#![allow(dead_code)]
#![allow(unused_variables)]
use anyhow::{bail, Result};
use ouroboros::self_referencing;
use smallvec::SmallVec;
use std::collections::BTreeMap;
use std::ops::Not;
use std::path::Path;

#[derive(Clone, Copy, Debug, Default)]
struct Header {
    _magic: u16,
    _version: u8,
    _flags: u8,
    _hdr_len: u32,
    type_off: u32,
    type_len: u32,
    str_off: u32,
    str_len: u32,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
#[repr(u8)]
pub enum TypeKind {
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

impl From<TypeKind> for u8 {
    fn from(value: TypeKind) -> Self {
        match value {
            TypeKind::Void => 0,
            TypeKind::Integer => 1,
            TypeKind::Pointer => 2,
            TypeKind::Array => 3,
            TypeKind::Struct => 4,
            TypeKind::Union => 5,
            TypeKind::Enum32 => 6,
            TypeKind::Fwd => 7,
            TypeKind::Typedef => 8,
            TypeKind::Volatile => 9,
            TypeKind::Const => 10,
            TypeKind::Restrict => 11,
            TypeKind::Function => 12,
            TypeKind::FunctionProto => 13,
            TypeKind::Variable => 14,
            TypeKind::DataSection => 15,
            TypeKind::Float => 16,
            TypeKind::DeclTag => 17,
            TypeKind::TypeTag => 18,
            TypeKind::Enum64 => 19,
        }
    }
}

macro_rules! match_enum_variants {
    ( $value:expr, $( $variant:ident ),* ) => {
        match $value {
            $(
                x if x == TypeKind::$variant as u8 => Some(TypeKind::$variant),
            )*
            _ => None,
        }
    };
}

impl TryFrom<u8> for TypeKind {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, ()> {
        match_enum_variants!(
            value,
            Array,
            Const,
            DataSection,
            DeclTag,
            Enum32,
            Enum64,
            Float,
            Function,
            FunctionProto,
            Fwd,
            Integer,
            Pointer,
            Restrict,
            Struct,
            TypeTag,
            Typedef,
            Union,
            Variable,
            Void,
            Volatile
        )
        .ok_or(())
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

impl<'a> TypeInfo<'a> {
    const fn size_checked<const KIND: u8>(&self) -> u32 {
        const {
            assert!(
                match_enum_variants!(KIND, Enum32, Enum64, Float, Integer, Struct, Union).is_some()
            )
        };
        self.size.unwrap()
    }

    const fn ref_type_checked<const KIND: u8>(&self) -> u32 {
        const {
            assert!(match_enum_variants!(
                KIND,
                Const,
                DeclTag,
                Function,
                FunctionProto,
                Pointer,
                Restrict,
                TypeTag,
                Typedef,
                Variable,
                Volatile
            )
            .is_some())
        };
        self.ref_type.unwrap()
    }

    fn get_size_or_type(kind: TypeKind, size_or_type: u32) -> (Option<u32>, Option<u32>) {
        match kind {
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
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct AggregateMember<'a> {
    pub name: Option<&'a str>,
    pub type_index: u32,
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
    pub type_index: u32,
}

#[derive(Clone, Copy, Debug, Default)]
pub struct SectionVariable {
    pub type_index: u32,
    pub offset: u32,
    pub size: u32,
}

#[derive(Clone, Debug, Default)]
pub struct Type<'a> {
    pub name: Option<&'a str>,
    pub ty: InnerType<'a>,
}

impl<'a> Type<'a> {
    fn kind(&self) -> TypeKind {
        self.ty.kind()
    }
}

#[derive(Clone, Debug, Default)]
pub enum InnerType<'a> {
    Array {
        elem_type_index: u32,
        index_type_index: u32,
        num_elements: u32,
    },
    Const(u32),
    DataSection(Box<[SectionVariable]>),
    DeclTag {
        type_index: u32,
        component_index: u32,
    },
    Enum32 {
        is_signed: bool,
        bytes: u32,
        entries: Box<[EnumEntry<'a>]>,
    },
    Enum64 {
        is_signed: bool,
        bytes: u32,
        entries: Box<[EnumEntry<'a>]>,
    },
    Float {
        bits: u32,
    },
    Function {
        linkage: Linkage,
        type_index: u32,
    },
    FunctionProto {
        ret: u32,
        args: Box<[FunctionParam<'a>]>,
    },
    Fwd(Fwd),
    Integer {
        used_bits: u32,
        bits: u32,
        is_signed: bool,
        is_char: bool,
        is_bool: bool,
    },
    Pointer(u32),
    Restrict(u32),
    Struct {
        bytes: u32,
        fields: Box<[AggregateMember<'a>]>,
    },
    TypeTag(u32),
    Typedef(u32),
    Union {
        bytes: u32,
        fields: Box<[AggregateMember<'a>]>,
    },
    Variable {
        linkage: Linkage,
        type_index: u32,
    },
    #[default]
    Void,
    Volatile(u32),
}

impl<'a> InnerType<'a> {
    fn kind(&self) -> TypeKind {
        match self {
            InnerType::Array { .. } => TypeKind::Array,
            InnerType::Const(_) => TypeKind::Const,
            InnerType::DataSection(_) => TypeKind::DataSection,
            InnerType::DeclTag { .. } => TypeKind::DeclTag,
            InnerType::Enum32 { .. } => TypeKind::Enum32,
            InnerType::Enum64 { .. } => TypeKind::Enum64,
            InnerType::Float { .. } => TypeKind::Float,
            InnerType::Function { .. } => TypeKind::Function,
            InnerType::FunctionProto { .. } => TypeKind::FunctionProto,
            InnerType::Fwd(_) => TypeKind::Fwd,
            InnerType::Integer { .. } => TypeKind::Integer,
            InnerType::Pointer(_) => TypeKind::Pointer,
            InnerType::Restrict(_) => TypeKind::Restrict,
            InnerType::Struct { .. } => TypeKind::Struct,
            InnerType::TypeTag(_) => TypeKind::TypeTag,
            InnerType::Typedef(_) => TypeKind::Typedef,
            InnerType::Union { .. } => TypeKind::Union,
            InnerType::Variable { .. } => TypeKind::Variable,
            InnerType::Void => TypeKind::Void,
            InnerType::Volatile(_) => TypeKind::Volatile,
        }
    }
}

#[derive(Clone, Copy, Debug, Default)]
enum Endianness {
    #[default]
    Little,
    Big,
}

struct Parser<'a> {
    data: &'a [u8],
    pos: usize,
}

impl<'a> Parser<'a> {
    fn new(data: &'a [u8]) -> Self {
        Parser { data, pos: 0 }
    }

    fn get_current(&self) -> &'a [u8] {
        &self.data[self.pos..]
    }

    fn remaining(&self) -> usize {
        self.data.len() - self.pos
    }

    fn read_u8(&mut self) -> Result<u8> {
        if self.pos + 1 > self.data.len() {
            bail!("Unexpected end of input");
        }
        let val = self.data[self.pos];
        self.pos += 1;
        Ok(val)
    }

    fn read_u16(&mut self, en: Endianness) -> Result<u16> {
        if self.pos + 2 > self.data.len() {
            bail!("Unexpected end of input");
        }
        let bytes = &self.data[self.pos..self.pos + 2];
        self.pos += 2;
        let val = match en {
            Endianness::Little => u16::from_le_bytes([bytes[0], bytes[1]]),
            Endianness::Big => u16::from_be_bytes([bytes[0], bytes[1]]),
        };
        Ok(val)
    }

    fn read_u32(&mut self, en: Endianness) -> Result<u32> {
        if self.pos + 4 > self.data.len() {
            bail!("Unexpected end of input");
        }
        let bytes = &self.data[self.pos..self.pos + 4];
        self.pos += 4;
        let val = match en {
            Endianness::Little => u32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]),
            Endianness::Big => u32::from_be_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]),
        };
        Ok(val)
    }

    fn read_i32(&mut self, en: Endianness) -> Result<i32> {
        self.read_u32(en).map(|v| v as i32)
    }

    fn read_bytes(&mut self, len: usize) -> Result<&'a [u8]> {
        if self.pos + len > self.data.len() {
            bail!("Unexpected end of input");
        }
        let bytes = &self.data[self.pos..self.pos + len];
        self.pos += len;
        Ok(bytes)
    }

    fn skip(&mut self, len: usize) -> Result<()> {
        if self.pos + len > self.data.len() {
            bail!("Unexpected end of input");
        }
        self.pos += len;
        Ok(())
    }

    fn set_pos(&mut self, pos: usize) -> Result<()> {
        if pos > self.data.len() {
            bail!("Position out of bounds");
        }
        self.pos = pos;
        Ok(())
    }
}

fn parse_header(input: &mut Parser<'_>) -> Result<(Endianness, Header)> {
    let magic = input.read_u16(Endianness::Little)?;
    let en = match magic {
        0xeb9f => Endianness::Little,
        0x9feb => Endianness::Big,
        _ => bail!("Invalid magic number: {:#x}", magic),
    };
    let _version = input.read_u8()?;
    let _flags = input.read_u8()?;
    let _hdr_len = input.read_u32(en)?;
    let type_off = input.read_u32(en)?;
    let type_len = input.read_u32(en)?;
    let str_off = input.read_u32(en)?;
    let str_len = input.read_u32(en)?;

    let header = Header {
        _magic: magic,
        _version,
        _flags,
        _hdr_len,
        type_off,
        type_len,
        str_off,
        str_len,
    };

    Ok((en, header))
}

fn read_str<'a>(strings: &'a [u8], offset: u32) -> Result<Option<&'a str>> {
    if offset as usize >= strings.len() {
        bail!("String offset out of bounds");
    }
    let s = &strings[offset as usize..];
    if let Some(end) = s.iter().position(|&b| b == 0) {
        let raw_str = &s[..end];
        let s = std::str::from_utf8(raw_str)?;
        Ok(s.is_empty().not().then_some(s))
    } else {
        bail!("Null terminator not found in strings section");
    }
}

fn parse_type_info<'a>(
    input: &mut Parser<'a>,
    strings: &'a [u8],

    en: Endianness,
) -> Result<(TypeKind, TypeInfo<'a>)> {
    let name_off = input.read_u32(en)?;
    let info = input.read_u32(en)?;
    let size_or_type = input.read_u32(en)?;

    let adjusted_name_off = name_off;
    let name = read_str(strings, adjusted_name_off)?;
    let kind_u8 = ((info >> 24) & 0x1f) as u8;
    let kind = TypeKind::try_from(kind_u8)
        .map_err(|_| anyhow::anyhow!("Invalid TypeKind value: {}", kind_u8))?;
    let (size, ref_type) = TypeInfo::get_size_or_type(kind, size_or_type);

    let type_info = TypeInfo {
        name,
        vlen: info as u16,
        kind_flag: ((info >> 16) & 0x1) == 0x1,
        size,
        ref_type,
    };
    Ok((kind, type_info))
}

fn parse_integer<'a>(
    input: &mut Parser<'a>,
    type_info: &TypeInfo<'a>,
    en: Endianness,
) -> Result<InnerType<'a>> {
    let kind_specific = input.read_u32(en)?;
    let bits = kind_specific as u8;
    let is_signed = ((kind_specific >> 24) & 0x1) == 0x1;
    let is_char = ((kind_specific >> 24) & 0x2) == 0x2;
    let is_bool = ((kind_specific >> 24) & 0x4) == 0x4;

    Ok(InnerType::Integer {
        bits: type_info.size_checked::<{ TypeKind::Integer as u8 }>() * 8,
        used_bits: bits.into(),
        is_signed,
        is_char,
        is_bool,
    })
}

fn parse_function<'a>(input: &mut Parser<'a>, type_info: &TypeInfo<'_>) -> Result<InnerType<'a>> {
    let linkage = if type_info.vlen == 0 {
        Linkage::Static
    } else {
        Linkage::Global
    };

    Ok(InnerType::Function {
        linkage,
        type_index: type_info.ref_type_checked::<{ TypeKind::Function as u8 }>(),
    })
}

fn parse_array<'a>(input: &mut Parser<'a>, en: Endianness) -> Result<InnerType<'a>> {
    let elem_type_index = input.read_u32(en)?;
    let index_type_index = input.read_u32(en)?;
    let num_elements = input.read_u32(en)?;

    Ok(InnerType::Array {
        elem_type_index,
        index_type_index,
        num_elements,
    })
}

fn parse_enum_entry32<'a>(
    input: &mut Parser<'a>,
    strings: &'a [u8],

    en: Endianness,
) -> Result<EnumEntry<'a>> {
    let name_offset = input.read_u32(en)?;
    let adjusted_name_off = name_offset;
    let name = read_str(strings, adjusted_name_off)?;
    let value = input.read_i32(en)? as i64;
    Ok(EnumEntry { name, value })
}

fn parse_enum_entry64<'a>(
    input: &mut Parser<'a>,
    strings: &'a [u8],

    en: Endianness,
) -> Result<EnumEntry<'a>> {
    let name_offset = input.read_u32(en)?;
    let adjusted_name_off = name_offset;
    let name = read_str(strings, adjusted_name_off)?;

    let low = input.read_i32(en)?;
    let high = input.read_i32(en)?;

    let value = low as i64 | ((high as i64) << 32);

    Ok(EnumEntry { name, value })
}

fn parse_enum32<'a>(
    input: &mut Parser<'a>,
    type_info: &TypeInfo<'a>,
    strings: &'a [u8],

    en: Endianness,
) -> Result<InnerType<'a>> {
    let is_signed = type_info.kind_flag;
    let num_entries = type_info.vlen as usize;
    let mut entries = Vec::with_capacity(num_entries);
    for _ in 0..num_entries {
        entries.push(parse_enum_entry32(input, strings, en)?);
    }
    let bytes = type_info.size_checked::<{ TypeKind::Enum32 as u8 }>();
    Ok(InnerType::Enum32 {
        is_signed,
        bytes,
        entries: entries.into_boxed_slice(),
    })
}

fn parse_enum64<'a>(
    input: &mut Parser<'a>,
    type_info: &TypeInfo<'a>,
    strings: &'a [u8],

    en: Endianness,
) -> Result<InnerType<'a>> {
    let is_signed = type_info.kind_flag;
    let num_entries = type_info.vlen as usize;
    let mut entries = Vec::with_capacity(num_entries);
    for _ in 0..num_entries {
        entries.push(parse_enum_entry64(input, strings, en)?);
    }
    let bytes = type_info.size_checked::<{ TypeKind::Enum64 as u8 }>();
    Ok(InnerType::Enum64 {
        is_signed,
        bytes,
        entries: entries.into_boxed_slice(),
    })
}

fn parse_struct_member<'a>(
    input: &mut Parser<'a>,
    type_info: &TypeInfo<'a>,
    strings: &'a [u8],

    en: Endianness,
) -> Result<AggregateMember<'a>> {
    let name_off = input.read_u32(en)?;
    let adjusted_name_off = name_off;
    let name = read_str(strings, adjusted_name_off)?;
    let type_index = input.read_u32(en)?;
    let offset_and_bits = input.read_u32(en)?;

    let (offset, bits) = if type_info.kind_flag {
        (offset_and_bits & 0x00ff_ffff, Some(offset_and_bits >> 24))
    } else {
        (offset_and_bits, None)
    };

    Ok(AggregateMember {
        name,
        type_index,
        offset,
        bits,
    })
}

fn parse_struct<'a>(
    input: &mut Parser<'a>,
    type_info: &TypeInfo<'a>,
    strings: &'a [u8],

    en: Endianness,
) -> Result<InnerType<'a>> {
    let num_members = type_info.vlen as usize;
    let mut members = Vec::with_capacity(num_members);
    for _ in 0..num_members {
        members.push(parse_struct_member(input, type_info, strings, en)?);
    }
    let bytes = type_info.size_checked::<{ TypeKind::Struct as u8 }>();
    Ok(InnerType::Struct {
        bytes,
        fields: members.into_boxed_slice(),
    })
}

fn parse_union<'a>(
    input: &mut Parser<'a>,
    type_info: &TypeInfo<'a>,
    strings: &'a [u8],

    en: Endianness,
) -> Result<InnerType<'a>> {
    let num_members = type_info.vlen as usize;
    let mut members = Vec::with_capacity(num_members);
    for _ in 0..num_members {
        members.push(parse_struct_member(input, type_info, strings, en)?);
    }
    let bytes = type_info.size_checked::<{ TypeKind::Union as u8 }>();
    Ok(InnerType::Union {
        bytes,
        fields: members.into_boxed_slice(),
    })
}

fn parse_function_param<'a>(
    input: &mut Parser<'a>,
    strings: &'a [u8],

    en: Endianness,
) -> Result<FunctionParam<'a>> {
    let name_off = input.read_u32(en)?;
    let type_index = input.read_u32(en)?;
    let name = if name_off == 0 {
        None
    } else {
        let adjusted_name_off = name_off;
        read_str(strings, adjusted_name_off)?
    };
    Ok(FunctionParam { name, type_index })
}

fn parse_function_proto<'a>(
    input: &mut Parser<'a>,
    type_info: &TypeInfo<'a>,
    strings: &'a [u8],

    en: Endianness,
) -> Result<InnerType<'a>> {
    let num_params = type_info.vlen as usize;
    let ret = type_info.ref_type_checked::<{ TypeKind::FunctionProto as u8 }>();
    let mut args = Vec::with_capacity(num_params);
    for _ in 0..num_params {
        args.push(parse_function_param(input, strings, en)?);
    }
    Ok(InnerType::FunctionProto {
        ret,
        args: args.into_boxed_slice(),
    })
}

fn parse_variable<'a>(
    input: &mut Parser<'a>,
    type_info: &TypeInfo<'a>,
    en: Endianness,
) -> Result<InnerType<'a>> {
    let value = input.read_u32(en)?;
    let linkage = match value {
        0 => Linkage::Static,
        1 => Linkage::Global,
        _ => bail!("Invalid linkage value: {}", value),
    };
    let type_index = type_info.ref_type_checked::<{ TypeKind::Variable as u8 }>();
    Ok(InnerType::Variable {
        type_index,
        linkage,
    })
}

fn parse_decl_tag<'a>(
    input: &mut Parser<'a>,
    type_info: &TypeInfo<'a>,
    en: Endianness,
) -> Result<InnerType<'a>> {
    let component_index = input.read_u32(en)?;
    let type_index = type_info.ref_type_checked::<{ TypeKind::DeclTag as u8 }>();
    Ok(InnerType::DeclTag {
        type_index,
        component_index,
    })
}

fn parse_section_variable(input: &mut Parser<'_>, en: Endianness) -> Result<SectionVariable> {
    let type_index = input.read_u32(en)?;
    let offset = input.read_u32(en)?;
    let size = input.read_u32(en)?;

    Ok(SectionVariable {
        type_index,
        offset,
        size,
    })
}

fn parse_data_section<'a>(
    input: &mut Parser<'a>,
    type_info: &TypeInfo<'a>,
    en: Endianness,
) -> Result<InnerType<'a>> {
    let num_vars = type_info.vlen as usize;
    let mut vars = Vec::with_capacity(num_vars);
    for _ in 0..num_vars {
        vars.push(parse_section_variable(input, en)?);
    }
    Ok(InnerType::DataSection(vars.into_boxed_slice()))
}

fn parse_type<'a>(
    input: &mut Parser<'a>,
    kind: TypeKind,
    type_info: &TypeInfo<'a>,
    strings: &'a [u8],

    en: Endianness,
) -> Result<InnerType<'a>> {
    match kind {
        TypeKind::Array => parse_array(input, en),
        TypeKind::Const => Ok(InnerType::Const(
            type_info.ref_type_checked::<{ TypeKind::Const as u8 }>(),
        )),
        TypeKind::DataSection => parse_data_section(input, type_info, en),
        TypeKind::DeclTag => parse_decl_tag(input, type_info, en),
        TypeKind::Enum32 => parse_enum32(input, type_info, strings, en),
        TypeKind::Enum64 => parse_enum64(input, type_info, strings, en),
        TypeKind::Float => Ok(InnerType::Float {
            bits: type_info.size_checked::<{ TypeKind::Float as u8 }>() * 8,
        }),
        TypeKind::Function => parse_function(input, type_info),
        TypeKind::FunctionProto => parse_function_proto(input, type_info, strings, en),
        TypeKind::Fwd => Ok(InnerType::Fwd(if type_info.kind_flag {
            Fwd::Union
        } else {
            Fwd::Struct
        })),
        TypeKind::Integer => parse_integer(input, type_info, en),
        TypeKind::Pointer => Ok(InnerType::Pointer(
            type_info.ref_type_checked::<{ TypeKind::Pointer as u8 }>(),
        )),
        TypeKind::Restrict => Ok(InnerType::Restrict(
            type_info.ref_type_checked::<{ TypeKind::Restrict as u8 }>(),
        )),
        TypeKind::Struct => parse_struct(input, type_info, strings, en),
        TypeKind::TypeTag => Ok(InnerType::TypeTag(
            type_info.ref_type_checked::<{ TypeKind::TypeTag as u8 }>(),
        )),
        TypeKind::Typedef => Ok(InnerType::Typedef(
            type_info.ref_type_checked::<{ TypeKind::Typedef as u8 }>(),
        )),
        TypeKind::Union => parse_union(input, type_info, strings, en),
        TypeKind::Variable => parse_variable(input, type_info, en),
        TypeKind::Void => Ok(InnerType::Void),
        TypeKind::Volatile => Ok(InnerType::Volatile(
            type_info.ref_type_checked::<{ TypeKind::Volatile as u8 }>(),
        )),
    }
}

fn parse_types<'a>(
    input: &'a [u8],
    type_off: u32,
    type_len: u32,
    strings: &'a [u8],
    en: Endianness,
) -> Result<Vec<Type<'a>>> {
    if type_off as usize > input.len() || type_off as usize + type_len as usize > input.len() {
        bail!("Type section out of bounds");
    }
    let types_slice = &input[type_off as usize..(type_off + type_len) as usize];
    let mut parser = Parser::new(types_slice);
    let mut types = vec![Type::default()];
    while parser.remaining() > 0 {
        let (kind, type_info) = parse_type_info(&mut parser, strings, en)?;
        let ty = parse_type(&mut parser, kind, &type_info, strings, en)?;
        let item = Type {
            name: type_info.name,
            ty,
        };
        types.push(item);
    }
    Ok(types)
}

fn parse(input: &[u8]) -> Result<Vec<Type<'_>>> {
    let mut parser = Parser::new(input);
    let (en, header) = parse_header(&mut parser)?;
    let input = parser.get_current();
    let strings = &input[header.str_off as usize..(header.str_off + header.str_len) as usize];
    parse_types(input, header.type_off, header.type_len, strings, en)
}

fn get_btf_types(data: &[u8]) -> Result<Box<[Type<'_>]>> {
    let types = parse(data)?;
    Ok(types.into_boxed_slice())
}

/// A map from names to types.
/// Names don't identify types uniquely, though is expected to be true for most types.
/// We store for each name a list of types, with their kind.
#[derive(Debug)]
pub struct NamesLookup<'a> {
    // we use a SmallVec with a single element since in > 99.7% of the cases we expect to have a single type per name
    names_lookup: BTreeMap<&'a str, SmallVec<[(u32, TypeKind); 1]>>,
}

impl<'a> NamesLookup<'a> {
    fn new() -> Self {
        Self {
            names_lookup: Default::default(),
        }
    }

    fn insert(&mut self, name: &'a str, index: u32, kind: TypeKind) {
        let tmp = self.names_lookup.entry(name).or_default();
        tmp.push((index, kind));
    }

    /// we expect to be able to identify for the majority of the cases the type by name and type kind,
    /// in case this isn't possible we bail we simply return an error
    fn get(&self, name: &str, kind: TypeKind) -> Result<Option<u32>> {
        let Some(types_per_name) = self.names_lookup.get(name) else {
            return Ok(None);
        };
        let Some((index, type_index)) = types_per_name
            .iter()
            .enumerate()
            .find(|(_, &(_, k))| k == kind)
            .map(|(index, &(type_index, _))| (index, type_index))
        else {
            return Ok(None);
        };
        if types_per_name[index + 1..].iter().any(|(_, k)| *k == kind) {
            bail!("Not unique type found for name: {}", name);
        };
        Ok(Some(type_index))
    }
}

#[derive(Debug)]
pub struct Btf<'a> {
    types_slice: Box<[Type<'a>]>,
}

impl<'a> Btf<'a> {
    pub fn new(bytes: &'a [u8]) -> Result<Self> {
        get_btf_types(bytes).map(|types_slice| Self { types_slice })
    }

    pub fn len(&self) -> usize {
        self.types_slice.len()
    }
}


impl<'a> std::ops::Index<usize> for Btf<'a> {
    type Output = Type<'a>;

    fn index(&self, index: usize) -> &Self::Output {
        &self.types_slice[index]
    }
}
