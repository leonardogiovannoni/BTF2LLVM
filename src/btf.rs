#![allow(dead_code)]
#![allow(unused_variables)]
use anyhow::{bail, Result};
use std::collections::BTreeMap;
use nom::bytes::complete::take;
use nom::combinator::{map, map_res};
use nom::error::ErrorKind;
use nom::multi::{count, fold_many0};
use nom::number::Endianness;
use nom::sequence::preceded;
use nom::Finish;
use nom::{
    bytes::complete::take_until,
    number::complete::i32,
    number::complete::{u16, u32, u8},
    sequence::tuple,
    IResult,
};
use ouroboros::self_referencing;
use rustc_hash::FxBuildHasher;
use smallvec::SmallVec;
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
    // try to move fail at compile time
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

fn parse_header(input: &[u8]) -> IResult<&[u8], (Endianness, Header)> {
    let (input, magic) = u16(Endianness::Little)(input)?;
    let en = match magic {
        0xeb9f => Endianness::Little,
        0x9feb => Endianness::Big,
        _ => {
            return Err(nom::Err::Error(nom::error::Error::new(
                input,
                ErrorKind::Tag,
            )))
        }
    };
    let x = map(
        tuple((u8, u8, u32(en), u32(en), u32(en), u32(en), u32(en))),
        |(_version, _flags, _hdr_len, type_off, type_len, str_off, str_len)| {
            (
                en,
                Header {
                    _magic: magic,
                    _version,
                    _flags,
                    _hdr_len,
                    type_off,
                    type_len,
                    str_off,
                    str_len,
                },
            )
        },
    )(input);
    x
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
    let kind: TypeKind = u8::try_from((info >> 24) & 0x1f)
        .unwrap()
        .try_into()
        .map_err(|_| nom::Err::Error(nom::error::Error::new(input, ErrorKind::Tag)))?;
    let (size, ref_type) = TypeInfo::get_size_or_type(kind, size_or_type);

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
            bits: type_info.size_checked::<{ TypeKind::Integer as u8 }>() * 8,
            used_bits: bits.into(),
            is_signed,
            is_char,
            is_bool,
        },
    ))
}

fn parse_function<'a>(input: &'a [u8], type_info: &TypeInfo<'_>) -> IResult<&'a [u8], InnerType<'a>> {
    let linkage = if type_info.vlen == 0 {
        Linkage::Static
    } else {
        Linkage::Global
    };

    Ok((
        input,
        InnerType::Function {
            linkage,
            type_index: type_info.ref_type_checked::<{ TypeKind::Function as u8 }>(),
        },
    ))
}

fn parse_array(input: &[u8], en: Endianness) -> IResult<&[u8], InnerType> {
    let (input, elem_type_index) = u32(en)(input)?;
    let (input, index_type_index) = u32(en)(input)?;
    let (input, num_elements) = u32(en)(input)?;

    Ok((
        input,
        InnerType::Array {
            elem_type_index,
            index_type_index,
            num_elements,
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
    let bytes = type_info.size_checked::<{ TypeKind::Enum32 as u8 }>();
    Ok((
        input,
        InnerType::Enum32 {
            is_signed,
            bytes,
            entries: entries.into_boxed_slice(),
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
    let bytes = type_info.size_checked::<{ TypeKind::Enum64 as u8 }>();
    Ok((
        input,
        InnerType::Enum64 {
            is_signed,
            bytes,
            entries: entries.into_boxed_slice(),
        },
    ))
}

fn parse_struct_member<'a>(
    input: &'a [u8],
    type_info: &TypeInfo<'a>,
    strings: &'a [u8],
    en: Endianness,
) -> IResult<&'a [u8], AggregateMember<'a>> {
    let (input, name_off) = u32(en)(input)?;
    let (input, name) = read_str(input, strings, name_off)?;
    let (input, type_index) = u32(en)(input)?;
    let (input, offset_and_bits) = u32(en)(input)?;

    let (offset, bits) = if type_info.kind_flag {
        (offset_and_bits & 0x00ff_ffff, Some(offset_and_bits >> 24))
    } else {
        (offset_and_bits, None)
    };

    Ok((
        input,
        AggregateMember {
            name,
            type_index,
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
    let num_members = type_info.vlen as usize;
    let (input, members) = count(
        |input| parse_struct_member(input, type_info, strings, en),
        num_members,
    )(input)?;
    let bytes = type_info.size_checked::<{ TypeKind::Struct as u8 }>();
    Ok((
        input,
        InnerType::Struct {
            bytes,
            fields: members.into_boxed_slice(),
        },
    ))
}

fn parse_union<'a>(
    input: &'a [u8],
    type_info: &TypeInfo<'a>,
    strings: &'a [u8],
    en: Endianness,
) -> IResult<&'a [u8], InnerType<'a>> {
    let num_members = type_info.vlen as usize;
    let (input, members) = count(
        |input| parse_struct_member(input, type_info, strings, en),
        num_members,
    )(input)?;
    let bytes = type_info.size_checked::<{ TypeKind::Union as u8 }>();
    Ok((
        input,
        InnerType::Union {
            bytes,
            fields: members.into_boxed_slice(),
        },
    ))
}

fn parse_function_param<'a>(
    input: &'a [u8],
    strings: &'a [u8],
    en: Endianness,
) -> IResult<&'a [u8], FunctionParam<'a>> {
    map(tuple((u32(en), u32(en))), move |(name_off, type_index)| {
        let (_, name) = read_str(input, strings, name_off).unwrap_or((input, None));
        FunctionParam { name, type_index }
    })(input)
}

fn parse_function_proto<'a>(
    input: &'a [u8],
    type_info: &TypeInfo<'a>,
    strings: &'a [u8],
    en: Endianness,
) -> IResult<&'a [u8], InnerType<'a>> {
    let num_params = type_info.vlen as usize;
    let ret = type_info.ref_type_checked::<{ TypeKind::FunctionProto as u8 }>();
    let (input, args) = count(|i| parse_function_param(i, strings, en), num_params)(input)?;
    Ok((
        input,
        InnerType::FunctionProto {
            ret,
            args: args.into_boxed_slice(),
        },
    ))
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
    let type_index = type_info.ref_type_checked::<{ TypeKind::Variable as u8 }>();
    Ok((
        input,
        InnerType::Variable {
            type_index,
            linkage,
        },
    ))
}

fn parse_decl_tag<'a>(
    input: &'a [u8],
    type_info: &TypeInfo<'a>,
    en: Endianness,
) -> IResult<&'a [u8], InnerType<'a>> {
    let (input, component_index) = u32(en)(input)?;
    let type_index = type_info.ref_type_checked::<{ TypeKind::DeclTag as u8 }>();
    Ok((
        input,
        InnerType::DeclTag {
            type_index,
            component_index,
        },
    ))
}

fn parse_section_variable(input: &[u8], en: Endianness) -> IResult<&[u8], SectionVariable> {
    let (input, type_index) = u32(en)(input)?;
    let (input, offset) = u32(en)(input)?;
    let (input, size) = u32(en)(input)?;

    Ok((
        input,
        SectionVariable {
            type_index,
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
    Ok((input, InnerType::DataSection(vars.into_boxed_slice())))
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
            InnerType::Const(type_info.ref_type_checked::<{ TypeKind::Const as u8 }>()),
        )),
        TypeKind::DataSection => parse_data_section(input, type_info, en),
        TypeKind::DeclTag => parse_decl_tag(input, type_info, en),
        TypeKind::Enum32 => parse_enum32(input, type_info, strings, en),
        TypeKind::Enum64 => parse_enum64(input, type_info, strings, en),
        TypeKind::Float => Ok((
            input,
            InnerType::Float {
                bits: type_info.size_checked::<{ TypeKind::Float as u8 }>() * 8,
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
            InnerType::Pointer(type_info.ref_type_checked::<{ TypeKind::Pointer as u8 }>()),
        )),
        TypeKind::Restrict => Ok((
            input,
            InnerType::Restrict(type_info.ref_type_checked::<{ TypeKind::Restrict as u8 }>()),
        )),
        TypeKind::Struct => parse_struct(input, type_info, strings, en),
        TypeKind::TypeTag => Ok((
            input,
            InnerType::TypeTag(type_info.ref_type_checked::<{ TypeKind::TypeTag as u8 }>()),
        )),
        TypeKind::Typedef => Ok((
            input,
            InnerType::Typedef(type_info.ref_type_checked::<{ TypeKind::Typedef as u8 }>()),
        )),
        TypeKind::Union => parse_union(input, type_info, strings, en),
        TypeKind::Variable => parse_variable(input, type_info, en),
        TypeKind::Void => Ok((input, InnerType::Void)),
        TypeKind::Volatile => Ok((
            input,
            InnerType::Volatile(type_info.ref_type_checked::<{ TypeKind::Volatile as u8 }>()),
        )),
    }
}

fn parse_types<'a>(
    input: &'a [u8],
    type_off: u32,
    type_len: u32,
    strings: &'a [u8],
    en: Endianness,
) -> IResult<&'a [u8], (Vec<Type<'a>>, NamesLookup<'a>)> {
    let (remaining_input, types) = preceded(take(type_off), take(type_len))(input)?;
    let (_, (types, names_lookup)) = fold_many0(
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
        || (vec![Type::default()], NamesLookup::new()),
        |(mut types, mut names_lookup), item| {
            if let Some(name) = item.name {
                names_lookup.insert(name, types.len() as u32, item.kind());
            }
            types.push(item);
            (types, names_lookup)
        },
    )(types)?;

    Ok((remaining_input, (types, names_lookup)))
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

fn parse(input: &[u8]) -> IResult<&[u8], (Vec<Type<'_>>, NamesLookup<'_>)> {
    let (input, (en, header)) = parse_header(input)?;
    let (_, strings) = preceded(take(header.str_off), take(header.str_len))(input)?;
    parse_types(input, header.type_off, header.type_len, strings, en)
}

fn get_btf_types(data: &[u8]) -> Result<(Box<[Type<'_>]>, NamesLookup<'_>)> {
    parse(data)
        .finish()
        .map(|(_, (types, lookup))| (types.into_boxed_slice(), lookup))
        .map_err(|e| anyhow::anyhow!("error parsing btf: {:?}", e.code))
}

#[derive(Debug)]
struct ParsedBtf<'a> {
    types_slice: Box<[Type<'a>]>,
    names_lookup: NamesLookup<'a>,
}

#[self_referencing]
#[derive(Debug)]
struct BtfCell {
    data: Box<[u8]>,
    #[borrows(data)]
    #[covariant]
    parsed_btf: ParsedBtf<'this>,
}

impl BtfCell {
    fn types(&self) -> &[Type] {
        &self.borrow_parsed_btf().types_slice
    }

    pub fn names_lookup(&self) -> &NamesLookup<'_> {
        &self.borrow_parsed_btf().names_lookup
    }
}

pub struct Btf(BtfCell);

impl Btf {
    pub fn new<P: AsRef<Path>>(path: P) -> Result<Self> {
        let data = std::fs::read(path).map_err(|e| anyhow::anyhow!("failed to read btf: {}", e))?;
        Ok(Self(BtfCell::try_new(data.into_boxed_slice(), |data| {
            get_btf_types(data).map(|(types_slice, names_lookup)| ParsedBtf {
                types_slice,
                names_lookup,
            })
        })?))
    }

    pub fn names(&self) -> impl Iterator<Item = &str> {
        self.0.names_lookup().names_lookup.keys().copied()
    }

    pub fn types(&self) -> &[Type<'_>] {
        self.0.types()
    }

    pub fn type_index_by_name(&self, name: &str, kind: TypeKind) -> Result<Option<u32>> {
        self.0.names_lookup().get(name, kind)
    }

    //pub fn type_by_name(&self, name: &str) -> Option<&Type> {
    //    self.type_index_by_name(name).map(|i| &self.types()[i])
    // }
}
