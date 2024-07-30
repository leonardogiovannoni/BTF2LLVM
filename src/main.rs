use libbpf_rs::btf::types::{Array, Enum, FuncProto, Int, Struct, Union};
use libbpf_rs::btf::{BtfKind, BtfType, TypeId};
use libbpf_rs::{btf_type_match, Btf, HasSize};

#[derive(Debug, Default)]
enum Type {
    Int(usize),
    Ptr(Box<Type>),
    Array {
        name: Option<String>,
        ty: Box<Type>,
        size: usize,
    },
    FuncProto {
        ret: Box<Type>,
        fields: Vec<Type>,
    },
    Struct {
        name: Option<String>,
    },

    Union {
        name: Option<String>,
    },
    Enum {
        name: Option<String>,
        fields: Vec<String>,
    },
    Void,
    Typedef {
        defined: String,
        orig: Box<Type>,
    },
    Fwd(String),
    Restrict(Box<Type>),
    Volatile(Box<Type>),
    Const(Box<Type>),
    Function {
        name: Option<String>,
        func_proto: Box<Type>,
    },
    #[default]
    Unknown,
}

impl Type {
    fn strip_typedef(&mut self) {
        match *self {
            Type::Typedef { ref mut orig, .. } => {
                let orig = orig.as_mut();
                orig.strip_typedef();
                *self = std::mem::take(orig);
            }
            Type::Ptr(ref mut ty) => ty.strip_typedef(),
            Type::Array { ref mut ty, .. } => ty.strip_typedef(),
            Type::FuncProto {
                ref mut ret,
                ref mut fields,
            } => {
                ret.strip_typedef();
                for field in fields {
                    field.strip_typedef();
                }
            }
            Type::Restrict(ref mut ty) => ty.strip_typedef(),
            Type::Volatile(ref mut ty) => ty.strip_typedef(),
            Type::Const(ref mut ty) => ty.strip_typedef(),
            Type::Function {
                ref mut func_proto, ..
            } => func_proto.strip_typedef(),
            _ => {}
        }
    }
}

trait BtfTypeExt {
    fn get_name(&self) -> Option<String>;
}

impl BtfTypeExt for BtfType<'_> {
    fn get_name(&self) -> Option<String> {
        self.name()
            .and_then(|n| n.to_str().ok())
            .map(|s| s.to_string())
    }
}

struct Parser<'a> {
    btf: &'a Btf<'a>,
    token: Option<BtfType<'a>>,
}

impl<'a> Parser<'a> {
    fn advance(&mut self) {
        self.token = self.token.map(|t| t.next_type()).flatten();
    }

    fn parse_proto(&mut self) -> Type {
        let token = self.token.unwrap();
        let func_proto: FuncProto = btf_type_match!(match token {
            BtfKind::FuncProto(func_proto) => func_proto,
            _ => unreachable!(),
        });
        self.advance();
        let ret_type = self.parse();
        let mut i = 0;
        let mut v = Vec::new();
        while let Some(param) = func_proto.get(i) {
            let ty: BtfType = self.btf.type_by_id(param.ty).unwrap();
            let mut parser = Parser {
                btf: &self.btf,
                token: Some(ty),
            };
            let param_type = parser.parse();
            v.push(param_type);
            i += 1;
        }
        self.advance();
        Type::FuncProto {
            ret: Box::new(ret_type),
            fields: v,
        }
    }

    fn parse_int(&mut self) -> Type {
        let token = self.token.unwrap();
        let type_: Int = btf_type_match!(match token {
            BtfKind::Int(type_) => type_,
            _ => unreachable!(),
        });
        let size = type_.size();

        Type::Int(size)
    }

    fn parse_fwd(&mut self) -> Type {
        self.advance();
        self.parse()
    }

    fn parse_typedef(&mut self) -> Type {
        let name = self.token.unwrap().get_name().unwrap();
        self.advance();
        Type::Typedef {
            defined: name,
            orig: Box::new(self.parse()),
        }
    }

    fn parse_restrict(&mut self) -> Type {
        self.advance();
        let ty = self.parse();
        Type::Restrict(Box::new(ty))
    }

    fn parse_volatile(&mut self) -> Type {
        self.advance();
        let ty = self.parse();
        Type::Volatile(Box::new(ty))
    }

    fn parse_ptr(&mut self) -> Type {
        self.advance();
        let ty = self.parse();
        Type::Ptr(Box::new(ty))
    }

    fn parse_array(&mut self) -> Type {
        let token = self.token.unwrap();
        let ty: Array = btf_type_match!(match token {
            BtfKind::Array(type_) => type_,
            _ => unreachable!(),
        });
        let name = ty
            .name()
            .map(|n| n.to_str().unwrap_or_default().to_string());
        let contained_ty = ty.contained_type();
        let size = ty.capacity();
        let mut parser = Parser {
            btf: &self.btf,
            token: Some(contained_ty),
        };
        let contained_ty = parser.parse();
        self.advance();
        Type::Array {
            ty: Box::new(contained_ty),
            size,
            name,
        }
    }

    fn parse_void(&mut self) -> Type {
        Type::Void
    }

    fn parse_struct(&mut self) -> Type {
        let token = self.token.unwrap();
        let type_: Struct = btf_type_match!(match token {
            BtfKind::Struct(type_) => type_,
            _ => unreachable!(),
        });
        let name = type_.get_name();
        Type::Struct { name }
    }

    fn parse_union(&mut self) -> Type {
        let token = self.token.unwrap();
        let type_: Union = btf_type_match!(match token {
            BtfKind::Union(type_) => type_,
            _ => unreachable!(),
        });
        let name = type_.get_name();

        Type::Union { name }
    }

    fn parse_enum(&mut self) -> Type {
        let mut v = Vec::new();
        let token = self.token.unwrap();
        let type_: Enum = btf_type_match!(match token {
            BtfKind::Enum(type_) => type_,
            _ => unreachable!(),
        });
        let mut i = 0;
        while let Some(field) = type_.get(i) {
            let tmp = field.name.map(|n| n.to_str().unwrap_or_default());
            // TODO: essere più esaustivo
            if let Some(name) = tmp {
                v.push(name.to_string());
            }
            i += 1;
        }
        self.advance();
        Type::Enum {
            fields: v,
            name: type_.get_name(),
        }
    }

    fn parse_const(&mut self) -> Type {
        self.advance();
        Type::Const(Box::new(self.parse()))
    }

    fn parse_func(&mut self) -> Type {
        let name = self.token.unwrap().get_name();
        self.advance();
        let ty = self.parse();
        Type::Function {
            func_proto: Box::new(ty),
            name,
        }
    }

    fn parse(&mut self) -> Type {
        match self.token.map(|x| x.kind()) {
            Some(BtfKind::FuncProto) => self.parse_proto(),
            Some(BtfKind::Int) => self.parse_int(),
            Some(BtfKind::Fwd) => self.parse_fwd(),
            Some(BtfKind::Typedef) => self.parse_typedef(),
            Some(BtfKind::Restrict) => self.parse_restrict(),
            Some(BtfKind::Volatile) => self.parse_volatile(),
            Some(BtfKind::Ptr) => self.parse_ptr(),
            Some(BtfKind::Array) => self.parse_array(),
            Some(BtfKind::Void) => self.parse_void(),
            Some(BtfKind::Struct) => self.parse_struct(),
            Some(BtfKind::Union) => self.parse_union(),
            Some(BtfKind::Enum) => self.parse_enum(),
            Some(BtfKind::Const) => self.parse_const(),
            Some(BtfKind::Func) => self.parse_func(),
            _ => {
                return Type::Unknown;
            }
        }
    }
}

fn pippo(btf: &Btf) {
    let mut i = 1;
    loop {
        let type_id = TypeId::from(i);
        let btf_type: Option<BtfType> = btf.type_by_id(type_id);
        match btf_type {
            Some(type_info) => {
                if type_info.kind() == BtfKind::Func {
                    let mut parser = Parser {
                        btf,
                        token: Some(type_info),
                    };

                    let mut ast = parser.parse();
                    ast.strip_typedef();
                    println!("{:#?}", ast);
                }
            }
            None => break,
        }
        i += 1;
    }
}

fn main() {
    let btf = Btf::from_vmlinux().unwrap();
    pippo(&btf);
} 
