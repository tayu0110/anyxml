use std::{
    borrow::Cow,
    sync::{Arc, LazyLock},
};

use crate::{
    XMLVersion,
    automata::xsregexp::XSRegexp,
    base64::Base64Binary,
    datetime::{Date, DateTime, Duration, GDay, GMonth, GMonthDay, GYear, GYearMonth, Time},
    error::XMLError,
    sax::NamespaceStack,
    uri::URIString,
    xsd::{Schema, XML_SCHEMA_NAMESPACE, XSDError, parse::TypeDefinition},
};

#[derive(Clone, Default)]
pub struct SimpleTypeDefinition {
    pub(super) name: Arc<str>,
    pub(super) target_namespace: Option<Arc<str>>,
    pub(super) base_type_definition: (Option<Arc<str>>, Arc<str>),
    pub(super) facets: Facets,
    pub(super) fundamental_facets: FundamentalFacets,
    pub(super) r#final: FinalSet,
    pub(super) variety: Option<Variety>,
}

impl SimpleTypeDefinition {
    pub fn display_name(&self) -> Cow<'_, str> {
        if let Some(ns) = self.target_namespace.as_deref() {
            Cow::Owned(format!("{{{}}}{}", ns, self.name))
        } else {
            Cow::Borrowed(&self.name)
        }
    }

    pub fn is_simple_ur_type(&self) -> bool {
        self.target_namespace.as_deref() == Some(XML_SCHEMA_NAMESPACE)
            && self.name.as_ref() == "anySimpleType"
    }

    pub fn is_builtin_type(&self) -> bool {
        self.target_namespace.as_deref() == Some(XML_SCHEMA_NAMESPACE)
            && matches!(
                self.name.as_ref(),
                "anySimpleType"
                    | "string"
                    | "boolean"
                    | "decimal"
                    | "float"
                    | "double"
                    | "duration"
                    | "dateTime"
                    | "time"
                    | "date"
                    | "gYearMonth"
                    | "gYear"
                    | "gMonthDay"
                    | "gDay"
                    | "gMonth"
                    | "hexBinary"
                    | "base64Binary"
                    | "anyURI"
                    | "QName"
                    | "NOTATION"
                    | "normalizedString"
                    | "token"
                    | "language"
                    | "NMTOKEN"
                    | "NMTOKENS"
                    | "Name"
                    | "NCName"
                    | "ID"
                    | "IDREF"
                    | "IDREFS"
                    | "ENTITY"
                    | "ENTITIES"
                    | "integer"
                    | "nonPositiveInteger"
                    | "negativeInteger"
                    | "long"
                    | "int"
                    | "short"
                    | "byte"
                    | "nonNegativeInteger"
                    | "unsignedLong"
                    | "unsignedInt"
                    | "unsignedShort"
                    | "unsignedByte"
                    | "positiveInteger"
            )
    }
}

#[derive(Clone, Default)]
pub(super) struct Facets {
    pub(super) length: Option<Length>,
    pub(super) min_length: Option<Length>,
    pub(super) max_length: Option<Length>,
    pub(super) pattern: Vec<Pattern>,
    pub(super) enumeration: Vec<Enumeration>,
    pub(super) whitespace: Option<WhitespaceConstraint>,
    pub(super) max_inclusive: Option<Boundary>,
    pub(super) max_exclusive: Option<Boundary>,
    pub(super) min_exclusive: Option<Boundary>,
    pub(super) min_inclusive: Option<Boundary>,
    pub(super) total_digits: Option<Length>,
    pub(super) fraction_digits: Option<Length>,
}

#[derive(Clone, Copy)]
pub(super) struct Length {
    pub(super) value: usize,
    pub(super) fixed: bool,
}

#[derive(Clone)]
pub(super) struct Pattern {
    pub(super) value: String,
}

#[derive(Clone)]
pub(super) struct Enumeration {
    pub(super) value: SchemaValue,
    // used for parsing enumerated values.
    pub(super) ns_context: NamespaceStack,
}

#[derive(Clone, Copy)]
pub(super) struct WhitespaceConstraint {
    pub(super) value: Whitespace,
    pub(super) fixed: bool,
}

/// The constraining facet 'whiteSpace'.
///
/// # Reference
/// - [4.3.6 whiteSpace](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#rf-whiteSpace)
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Whitespace {
    /// 'preserve'
    Preserve = 0,
    /// 'replace'
    Replace = 1,
    /// 'collapse'
    Collapse = 2,
}

impl std::fmt::Display for Whitespace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Whitespace::Preserve => write!(f, "preserve"),
            Whitespace::Replace => write!(f, "replace"),
            Whitespace::Collapse => write!(f, "collapse"),
        }
    }
}

#[derive(Clone, Default)]
pub(super) struct Boundary {
    pub(super) value: SchemaValue,
    pub(super) fixed: bool,
    // used for parsing boundary facets
    // (minInclusive, minExclusive, maxInclusive and maxExclusive)
    pub(super) ns_context: NamespaceStack,
}

#[derive(Clone, Default)]
pub(super) struct FundamentalFacets {
    pub(super) ordered: Ordered,
    pub(super) bounded: bool,
    pub(super) cardinality: Cardinality,
    pub(super) numeric: bool,
}

#[derive(Clone, Copy, Default)]
pub(super) enum Ordered {
    #[default]
    False,
    Partial,
    Total,
}

#[derive(Clone, Copy, Default)]
pub(super) enum Cardinality {
    #[default]
    CountablyInfinite,
    Finite,
}

#[derive(Debug, Clone)]
pub(super) enum Variety {
    Atomic(Arc<str>),
    List(Option<Arc<str>>, Arc<str>),
    Union(Arc<[(Option<Arc<str>>, Arc<str>)]>),
}

#[derive(Clone, Copy)]
pub enum Final {
    /// 'extension'
    Extension = 1,
    /// 'restriction'
    Restriction = 2,
    /// 'list'
    List = 4,
    /// 'union'
    Union = 8,
}

impl std::ops::BitOr for Final {
    type Output = FinalSet;
    fn bitor(self, rhs: Self) -> Self::Output {
        FinalSet(self as u8 | rhs as u8)
    }
}

impl std::ops::BitOr<FinalSet> for Final {
    type Output = FinalSet;
    fn bitor(self, rhs: FinalSet) -> Self::Output {
        FinalSet(self as u8 | rhs.0)
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct FinalSet(u8);

impl FinalSet {
    /// Check if 'list' is contained.
    pub fn list(&self) -> bool {
        self.0 & Final::List as u8 != 0
    }

    /// Check if 'union' is contained.
    pub fn union(&self) -> bool {
        self.0 & Final::Union as u8 != 0
    }

    /// Check if 'restriction' is contained.
    pub fn restriction(&self) -> bool {
        self.0 & Final::Restriction as u8 != 0
    }

    /// Check if 'extension' is contained.
    pub fn extension(&self) -> bool {
        self.0 & Final::Extension as u8 != 0
    }
}

impl std::ops::BitOr<Final> for FinalSet {
    type Output = FinalSet;
    fn bitor(self, rhs: Final) -> Self::Output {
        FinalSet(self.0 | rhs as u8)
    }
}

impl std::ops::BitOrAssign<Final> for FinalSet {
    fn bitor_assign(&mut self, rhs: Final) {
        *self = *self | rhs;
    }
}

#[derive(Clone, Copy)]
pub enum Block {
    /// 'extension'
    Extension = 1,
    /// 'restriction'
    Restriction = 2,
    /// 'substitution'
    Substitution = 4,
}

impl std::ops::BitOr for Block {
    type Output = BlockSet;
    fn bitor(self, rhs: Self) -> Self::Output {
        BlockSet(self as u8 | rhs as u8)
    }
}

impl std::ops::BitOr<BlockSet> for Block {
    type Output = BlockSet;
    fn bitor(self, rhs: BlockSet) -> Self::Output {
        BlockSet(self as u8 | rhs.0)
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct BlockSet(u8);

impl BlockSet {
    pub fn extension(&self) -> bool {
        self.0 & Block::Extension as u8 != 0
    }

    pub fn restriction(&self) -> bool {
        self.0 & Block::Restriction as u8 != 0
    }

    pub fn substitution(&self) -> bool {
        self.0 & Block::Substitution as u8 != 0
    }

    pub fn is_subset_of(&self, base: &Self) -> bool {
        self.0 & base.0 == self.0
    }
}

impl std::ops::BitOr<Block> for BlockSet {
    type Output = BlockSet;
    fn bitor(self, rhs: Block) -> Self::Output {
        BlockSet(self.0 | rhs as u8)
    }
}

impl std::ops::BitOrAssign<Block> for BlockSet {
    fn bitor_assign(&mut self, rhs: Block) {
        *self = *self | rhs;
    }
}

pub(super) static XSD_ANY_SIMPLE_TYPE: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let xsd_ns: Arc<str> = Arc::from(XML_SCHEMA_NAMESPACE);
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: Arc::from("anySimpleType"),
        target_namespace: Some(xsd_ns.clone()),
        base_type_definition: (Some(xsd_ns), Arc::from("anyType")),
        facets: Facets::default(),
        fundamental_facets: FundamentalFacets::default(),
        r#final: FinalSet::default(),
        variety: None,
    }))
});
static XSD_STRING: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("string");
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: name.clone(),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            XSD_ANY_SIMPLE_TYPE.name(),
        ),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Preserve,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::False,
            ..Default::default()
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::Atomic(name)),
    }))
});
static XSD_BOOLEAN: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("boolean");
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: name.clone(),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            XSD_ANY_SIMPLE_TYPE.name(),
        ),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Collapse,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::False,
            cardinality: Cardinality::Finite,
            ..Default::default()
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::Atomic(name)),
    }))
});
static XSD_DECIMAL: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("decimal");
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: name.clone(),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            XSD_ANY_SIMPLE_TYPE.name(),
        ),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Collapse,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::Total,
            numeric: true,
            ..Default::default()
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::Atomic(name)),
    }))
});
static XSD_FLOAT: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("float");
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: name.clone(),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            XSD_ANY_SIMPLE_TYPE.name(),
        ),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Collapse,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::Partial,
            bounded: true,
            cardinality: Cardinality::Finite,
            numeric: true,
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::Atomic(name)),
    }))
});
static XSD_DOUBLE: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("double");
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: name.clone(),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            XSD_ANY_SIMPLE_TYPE.name(),
        ),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Collapse,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::Partial,
            bounded: true,
            cardinality: Cardinality::Finite,
            numeric: true,
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::Atomic(name)),
    }))
});
static XSD_DURATION: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("duration");
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: name.clone(),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            XSD_ANY_SIMPLE_TYPE.name(),
        ),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Collapse,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::Partial,
            ..Default::default()
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::Atomic(name)),
    }))
});
static XSD_DATETIME: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("dateTime");
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: name.clone(),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            XSD_ANY_SIMPLE_TYPE.name(),
        ),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Collapse,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::Partial,
            ..Default::default()
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::Atomic(name)),
    }))
});
static XSD_TIME: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("time");
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: name.clone(),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            XSD_ANY_SIMPLE_TYPE.name(),
        ),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Collapse,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::Partial,
            ..Default::default()
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::Atomic(name)),
    }))
});
static XSD_DATE: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("date");
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: name.clone(),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            XSD_ANY_SIMPLE_TYPE.name(),
        ),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Collapse,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::Partial,
            ..Default::default()
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::Atomic(name)),
    }))
});
static XSD_GYEARMONTH: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("gYearMonth");
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: name.clone(),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            XSD_ANY_SIMPLE_TYPE.name(),
        ),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Collapse,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::Partial,
            ..Default::default()
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::Atomic(name)),
    }))
});
static XSD_GYEAR: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("gYear");
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: name.clone(),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            XSD_ANY_SIMPLE_TYPE.name(),
        ),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Collapse,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::Partial,
            ..Default::default()
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::Atomic(name)),
    }))
});
static XSD_GMONTHDAY: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("gMonthDay");
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: name.clone(),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            XSD_ANY_SIMPLE_TYPE.name(),
        ),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Collapse,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::Partial,
            ..Default::default()
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::Atomic(name)),
    }))
});
static XSD_GDAY: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("gDay");
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: name.clone(),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            XSD_ANY_SIMPLE_TYPE.name(),
        ),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Collapse,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::Partial,
            ..Default::default()
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::Atomic(name)),
    }))
});
static XSD_GMONTH: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("gMonth");
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: name.clone(),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            XSD_ANY_SIMPLE_TYPE.name(),
        ),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Collapse,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::Partial,
            ..Default::default()
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::Atomic(name)),
    }))
});
static XSD_HEXBINARY: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("hexBinary");
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: name.clone(),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            XSD_ANY_SIMPLE_TYPE.name(),
        ),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Collapse,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::False,
            ..Default::default()
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::Atomic(name)),
    }))
});
static XSD_BASE64BINARY: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("base64Binary");
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: name.clone(),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            XSD_ANY_SIMPLE_TYPE.name(),
        ),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Collapse,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::False,
            ..Default::default()
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::Atomic(name)),
    }))
});
static XSD_ANYURI: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("anyURI");
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: name.clone(),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            XSD_ANY_SIMPLE_TYPE.name(),
        ),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Collapse,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::False,
            ..Default::default()
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::Atomic(name)),
    }))
});
static XSD_QNAME: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("QName");
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: name.clone(),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            XSD_ANY_SIMPLE_TYPE.name(),
        ),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Collapse,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::False,
            ..Default::default()
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::Atomic(name)),
    }))
});
static XSD_NOTATION: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("NOTATION");
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: name.clone(),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            XSD_ANY_SIMPLE_TYPE.name(),
        ),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Collapse,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::False,
            ..Default::default()
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::Atomic(name)),
    }))
});
static XSD_NORMALIZED_STRING: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: Arc::from("normalizedString"),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (XSD_ANY_SIMPLE_TYPE.target_namespace(), XSD_STRING.name()),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Preserve,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::False,
            ..Default::default()
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::Atomic(XSD_STRING.name())),
    }))
});
static XSD_TOKEN: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: Arc::from("token"),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            XSD_NORMALIZED_STRING.name(),
        ),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Preserve,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::False,
            ..Default::default()
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::Atomic(XSD_STRING.name())),
    }))
});
static XSD_LANGUAGE: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: Arc::from("language"),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (XSD_ANY_SIMPLE_TYPE.target_namespace(), XSD_TOKEN.name()),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Preserve,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::False,
            ..Default::default()
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::Atomic(XSD_STRING.name())),
    }))
});
static XSD_NMTOKEN: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: Arc::from("NMTOKEN"),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (XSD_ANY_SIMPLE_TYPE.target_namespace(), XSD_TOKEN.name()),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Preserve,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::False,
            ..Default::default()
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::Atomic(XSD_STRING.name())),
    }))
});
static XSD_NMTOKENS: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: Arc::from("NMTOKENS"),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            XSD_ANY_SIMPLE_TYPE.name(),
        ),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Collapse,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::False,
            ..Default::default()
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::List(
            XSD_TOKEN.target_namespace(),
            XSD_TOKEN.name(),
        )),
    }))
});
static XSD_NAME: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: Arc::from("Name"),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (XSD_ANY_SIMPLE_TYPE.target_namespace(), XSD_TOKEN.name()),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Preserve,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::False,
            ..Default::default()
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::Atomic(XSD_STRING.name())),
    }))
});
static XSD_NCNAME: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: Arc::from("NCName"),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (XSD_ANY_SIMPLE_TYPE.target_namespace(), XSD_NAME.name()),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Preserve,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::False,
            ..Default::default()
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::Atomic(XSD_STRING.name())),
    }))
});
static XSD_ID: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: Arc::from("ID"),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (XSD_ANY_SIMPLE_TYPE.target_namespace(), XSD_NCNAME.name()),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Preserve,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::False,
            ..Default::default()
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::Atomic(XSD_STRING.name())),
    }))
});
static XSD_IDREF: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: Arc::from("IDREF"),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (XSD_ANY_SIMPLE_TYPE.target_namespace(), XSD_NCNAME.name()),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Preserve,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::False,
            ..Default::default()
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::Atomic(XSD_STRING.name())),
    }))
});
static XSD_IDREFS: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: Arc::from("IDREFS"),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            XSD_ANY_SIMPLE_TYPE.name(),
        ),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Collapse,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::False,
            ..Default::default()
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::List(
            XSD_IDREF.target_namespace(),
            XSD_IDREF.name(),
        )),
    }))
});
static XSD_ENTITY: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: Arc::from("ENTITY"),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (XSD_ANY_SIMPLE_TYPE.target_namespace(), XSD_NCNAME.name()),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Preserve,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::False,
            ..Default::default()
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::Atomic(XSD_STRING.name())),
    }))
});
static XSD_ENTITIES: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: Arc::from("ENTITIES"),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            XSD_ANY_SIMPLE_TYPE.name(),
        ),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Collapse,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::False,
            ..Default::default()
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::List(
            XSD_ENTITY.target_namespace(),
            XSD_ENTITY.name(),
        )),
    }))
});
static XSD_INTEGER: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: Arc::from("integer"),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (XSD_ANY_SIMPLE_TYPE.target_namespace(), XSD_DECIMAL.name()),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Collapse,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::Total,
            numeric: true,
            ..Default::default()
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::Atomic(XSD_DECIMAL.name())),
    }))
});
static XSD_NONPOSITIVE_INTEGER: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: Arc::from("nonPositiveInteger"),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (XSD_ANY_SIMPLE_TYPE.target_namespace(), XSD_INTEGER.name()),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Collapse,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::Total,
            numeric: true,
            ..Default::default()
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::Atomic(XSD_DECIMAL.name())),
    }))
});
static XSD_NEGATIVE_INTEGER: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: Arc::from("negativeInteger"),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            XSD_NONPOSITIVE_INTEGER.name(),
        ),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Collapse,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::Total,
            numeric: true,
            ..Default::default()
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::Atomic(XSD_DECIMAL.name())),
    }))
});
static XSD_LONG: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: Arc::from("long"),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (XSD_ANY_SIMPLE_TYPE.target_namespace(), XSD_INTEGER.name()),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Collapse,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::Total,
            bounded: true,
            cardinality: Cardinality::Finite,
            numeric: true,
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::Atomic(XSD_DECIMAL.name())),
    }))
});
static XSD_INT: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: Arc::from("int"),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (XSD_ANY_SIMPLE_TYPE.target_namespace(), XSD_LONG.name()),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Collapse,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::Total,
            bounded: true,
            cardinality: Cardinality::Finite,
            numeric: true,
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::Atomic(XSD_DECIMAL.name())),
    }))
});
static XSD_SHORT: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: Arc::from("short"),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (XSD_ANY_SIMPLE_TYPE.target_namespace(), XSD_INT.name()),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Collapse,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::Total,
            bounded: true,
            cardinality: Cardinality::Finite,
            numeric: true,
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::Atomic(XSD_DECIMAL.name())),
    }))
});
static XSD_BYTE: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: Arc::from("byte"),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (XSD_ANY_SIMPLE_TYPE.target_namespace(), XSD_SHORT.name()),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Collapse,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::Total,
            bounded: true,
            cardinality: Cardinality::Finite,
            numeric: true,
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::Atomic(XSD_DECIMAL.name())),
    }))
});
static XSD_NONNEGATIVE_INTEGER: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: Arc::from("nonNegativeInteger"),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (XSD_ANY_SIMPLE_TYPE.target_namespace(), XSD_INTEGER.name()),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Collapse,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::Total,
            numeric: true,
            ..Default::default()
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::Atomic(XSD_DECIMAL.name())),
    }))
});
static XSD_UNSIGNED_LONG: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: Arc::from("unsignedLong"),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            XSD_NONNEGATIVE_INTEGER.name(),
        ),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Collapse,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::Total,
            bounded: true,
            cardinality: Cardinality::Finite,
            numeric: true,
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::Atomic(XSD_DECIMAL.name())),
    }))
});
static XSD_UNSIGNED_INT: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: Arc::from("unsignedInt"),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            XSD_UNSIGNED_LONG.name(),
        ),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Collapse,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::Total,
            bounded: true,
            cardinality: Cardinality::Finite,
            numeric: true,
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::Atomic(XSD_DECIMAL.name())),
    }))
});
static XSD_UNSIGNED_SHORT: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: Arc::from("unsignedShort"),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            XSD_UNSIGNED_INT.name(),
        ),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Collapse,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::Total,
            bounded: true,
            cardinality: Cardinality::Finite,
            numeric: true,
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::Atomic(XSD_DECIMAL.name())),
    }))
});
static XSD_UNSIGNED_BYTE: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: Arc::from("unsignedByte"),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            XSD_UNSIGNED_SHORT.name(),
        ),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Collapse,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::Total,
            bounded: true,
            cardinality: Cardinality::Finite,
            numeric: true,
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::Atomic(XSD_DECIMAL.name())),
    }))
});
static XSD_POSITIVE_INTEGER: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
        name: Arc::from("positiveInteger"),
        target_namespace: XSD_ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            XSD_NONNEGATIVE_INTEGER.name(),
        ),
        facets: Facets {
            whitespace: Some(WhitespaceConstraint {
                value: Whitespace::Collapse,
                fixed: false,
            }),
            ..Default::default()
        },
        fundamental_facets: FundamentalFacets {
            ordered: Ordered::Total,
            numeric: true,
            ..Default::default()
        },
        r#final: FinalSet::default(),
        variety: Some(Variety::Atomic(XSD_DECIMAL.name())),
    }))
});

pub(super) fn get_builtin_simple_type(
    target_namespace: Option<&str>,
    name: &str,
) -> Option<Arc<SimpleTypeDefinition>> {
    if target_namespace != Some(XML_SCHEMA_NAMESPACE) {
        return None;
    }

    match name {
        "anySimpleType" => XSD_ANY_SIMPLE_TYPE.as_simple_type(),
        "anyURI" => XSD_ANYURI.as_simple_type(),
        "base64Binary" => XSD_BASE64BINARY.as_simple_type(),
        "boolean" => XSD_BOOLEAN.as_simple_type(),
        "byte" => XSD_BYTE.as_simple_type(),
        "date" => XSD_DATE.as_simple_type(),
        "dateTime" => XSD_DATETIME.as_simple_type(),
        "decimal" => XSD_DECIMAL.as_simple_type(),
        "double" => XSD_DOUBLE.as_simple_type(),
        "duration" => XSD_DURATION.as_simple_type(),
        "ENTITIES" => XSD_ENTITIES.as_simple_type(),
        "ENTITY" => XSD_ENTITY.as_simple_type(),
        "float" => XSD_FLOAT.as_simple_type(),
        "gDay" => XSD_GDAY.as_simple_type(),
        "gMonth" => XSD_GMONTH.as_simple_type(),
        "gMonthDay" => XSD_GMONTHDAY.as_simple_type(),
        "gYear" => XSD_GYEAR.as_simple_type(),
        "gYearMonth" => XSD_GYEARMONTH.as_simple_type(),
        "hexBinary" => XSD_HEXBINARY.as_simple_type(),
        "ID" => XSD_ID.as_simple_type(),
        "IDREF" => XSD_IDREF.as_simple_type(),
        "IDREFS" => XSD_IDREFS.as_simple_type(),
        "int" => XSD_INT.as_simple_type(),
        "integer" => XSD_INTEGER.as_simple_type(),
        "language" => XSD_LANGUAGE.as_simple_type(),
        "long" => XSD_LONG.as_simple_type(),
        "NCName" => XSD_NCNAME.as_simple_type(),
        "NMTOKEN" => XSD_NMTOKEN.as_simple_type(),
        "NMTOKENS" => XSD_NMTOKENS.as_simple_type(),
        "NOTATION" => XSD_NOTATION.as_simple_type(),
        "Name" => XSD_NAME.as_simple_type(),
        "negativeInteger" => XSD_NEGATIVE_INTEGER.as_simple_type(),
        "nonNegativeInteger" => XSD_NONNEGATIVE_INTEGER.as_simple_type(),
        "nonPositiveInteger" => XSD_NONPOSITIVE_INTEGER.as_simple_type(),
        "normalizedString" => XSD_NORMALIZED_STRING.as_simple_type(),
        "positiveInteger" => XSD_POSITIVE_INTEGER.as_simple_type(),
        "QName" => XSD_QNAME.as_simple_type(),
        "short" => XSD_SHORT.as_simple_type(),
        "string" => XSD_STRING.as_simple_type(),
        "time" => XSD_TIME.as_simple_type(),
        "token" => XSD_TOKEN.as_simple_type(),
        "unsignedByte" => XSD_UNSIGNED_BYTE.as_simple_type(),
        "unsignedInt" => XSD_UNSIGNED_INT.as_simple_type(),
        "unsignedLong" => XSD_UNSIGNED_LONG.as_simple_type(),
        "unsignedShort" => XSD_UNSIGNED_SHORT.as_simple_type(),
        _ => None,
    }
}

impl Schema {
    pub(super) fn common_ancestor_of_simple_types(
        &self,
        types: &[Arc<SimpleTypeDefinition>],
    ) -> Arc<SimpleTypeDefinition> {
        if types.is_empty() {
            return XSD_ANY_SIMPLE_TYPE.as_simple_type().unwrap();
        } else if types.len() == 1 {
            return types[0].clone();
        }

        let create_stack =
            |t: Arc<SimpleTypeDefinition>| -> Option<Vec<Arc<SimpleTypeDefinition>>> {
                let mut stack = vec![t.clone()];
                while let Some(t) = stack.last()
                    && !t.is_simple_ur_type()
                {
                    let base = t.base_type_definition.clone();
                    let base = self.type_defs.get(&base).and_then(|t| t.as_simple_type())?;
                    stack.push(base);
                }
                stack.reverse();
                Some(stack)
            };
        let Some(mut stack) = create_stack(types[0].clone()) else {
            return XSD_ANY_SIMPLE_TYPE.as_simple_type().unwrap();
        };
        for t in types.iter().skip(1).cloned() {
            let Some(tmp) = create_stack(t) else {
                return XSD_ANY_SIMPLE_TYPE.as_simple_type().unwrap();
            };
            let min = stack.len().min(tmp.len());
            let pos = stack
                .iter()
                .zip(tmp)
                .position(|(s, t)| s.target_namespace != t.target_namespace || s.name != t.name)
                .unwrap_or(min);
            if pos == 1 {
                return XSD_ANY_SIMPLE_TYPE.as_simple_type().unwrap();
            }
            stack.truncate(pos);
        }
        stack.pop().unwrap()
    }

    pub(super) fn parse_simple_type(
        &self,
        value: &str,
        target_namespace: Option<&str>,
        name: &str,
        ns_context: &NamespaceStack,
    ) -> Result<SchemaValue, XMLError> {
        if let Some(t) = get_builtin_simple_type(target_namespace, name) {
            let collapsed = normalize_whitespace(value, Whitespace::Collapse);
            match t.name.as_ref() {
                "anySimpleType" | "string" => {
                    return Ok(SchemaValue {
                        r#type: SchemaValueType::String,
                        list: false,
                        raw_value: value.into(),
                        canonic_value: value.into(),
                    });
                }
                "boolean" => {
                    let canonic = match collapsed.as_ref() {
                        "true" | "1" => "true",
                        "false" | "0" => "false",
                        _ => return Err(XMLError::XSDError(XSDError::CvcSimpleType)),
                    };
                    return Ok(SchemaValue {
                        r#type: SchemaValueType::Boolean,
                        list: false,
                        raw_value: collapsed.into(),
                        canonic_value: canonic.into(),
                    });
                }
                "decimal" => {
                    // 3.2.3.1 Lexical representation
                    let mut cur = collapsed.as_ref();
                    let mut canonic = String::with_capacity(collapsed.len());
                    if let Some(rem) = collapsed.strip_prefix('-') {
                        canonic.push('-');
                        cur = rem;
                    } else if let Some(rem) = collapsed.strip_prefix('+') {
                        cur = rem;
                    }
                    if !cur.starts_with(|c: char| c.is_ascii_digit()) {
                        return Err(XMLError::XSDError(XSDError::CvcSimpleType));
                    }
                    cur = cur.trim_start_matches('0');
                    if cur.starts_with('.') {
                        canonic.push('0');
                        canonic.push_str(cur);
                    } else {
                        canonic.push_str(cur);
                        cur = cur.trim_start_matches(|c: char| c.is_ascii_digit());
                    }
                    if let Some(rem) = cur.strip_prefix('.') {
                        cur = rem
                            .strip_prefix(|c: char| c.is_ascii_digit())
                            .ok_or(XSDError::CvcSimpleType)?
                            .trim_start_matches(|c: char| c.is_ascii_digit());
                    } else {
                        canonic.push_str(".0");
                    }
                    while canonic.ends_with('0') && !canonic.ends_with(".0") {
                        canonic.pop();
                    }
                    if !cur.is_empty() {
                        return Err(XMLError::XSDError(XSDError::CvcSimpleType));
                    }
                    if canonic == "-0.0" {
                        canonic.remove(0);
                    }
                    return Ok(SchemaValue {
                        r#type: SchemaValueType::Decimal,
                        list: false,
                        raw_value: collapsed.into(),
                        canonic_value: canonic.into(),
                    });
                }
                "float" => {
                    if matches!(collapsed.as_ref(), "INF" | "-INF" | "NaN") {
                        let raw_value: Box<str> = collapsed.into();
                        return Ok(SchemaValue {
                            r#type: SchemaValueType::Float,
                            list: false,
                            canonic_value: raw_value.clone(),
                            raw_value,
                        });
                    }
                    let f = collapsed
                        .parse::<f32>()
                        .map_err(|_| XSDError::CvcSimpleType)?;
                    return if f.is_infinite() || f.is_nan() {
                        Err(XMLError::XSDError(XSDError::CvcSimpleType))
                    } else {
                        let mut canonic = format!("{:1E}", f);
                        if !canonic.contains('.') {
                            canonic = canonic.replace('E', ".0E");
                        }
                        Ok(SchemaValue {
                            r#type: SchemaValueType::Float,
                            list: false,
                            raw_value: collapsed.into(),
                            canonic_value: canonic.into(),
                        })
                    };
                }
                "double" => {
                    if matches!(collapsed.as_ref(), "INF" | "-INF" | "NaN") {
                        let raw_value: Box<str> = collapsed.into();
                        return Ok(SchemaValue {
                            r#type: SchemaValueType::Double,
                            list: false,
                            canonic_value: raw_value.clone(),
                            raw_value,
                        });
                    }
                    let f = collapsed
                        .parse::<f64>()
                        .map_err(|_| XSDError::CvcSimpleType)?;
                    return if f.is_infinite() || f.is_nan() {
                        Err(XMLError::XSDError(XSDError::CvcSimpleType))
                    } else {
                        let mut canonic = format!("{:1E}", f);
                        if !canonic.contains('.') {
                            canonic = canonic.replace('E', ".0E");
                        }
                        Ok(SchemaValue {
                            r#type: SchemaValueType::Double,
                            list: false,
                            raw_value: collapsed.into(),
                            canonic_value: canonic.into(),
                        })
                    };
                }
                "duration" => {
                    let dur = collapsed
                        .parse::<Duration>()
                        .map_err(|_| XSDError::CvcSimpleType)?;
                    return Ok(SchemaValue {
                        r#type: SchemaValueType::Duration,
                        list: false,
                        raw_value: collapsed.into(),
                        canonic_value: dur.to_string().into(),
                    });
                }
                "dateTime" => {
                    let dur = collapsed
                        .parse::<DateTime>()
                        .map_err(|_| XSDError::CvcSimpleType)?;
                    return Ok(SchemaValue {
                        r#type: SchemaValueType::DateTime,
                        list: false,
                        raw_value: collapsed.into(),
                        canonic_value: dur.to_string().into(),
                    });
                }
                "time" => {
                    let dur = collapsed
                        .parse::<Time>()
                        .map_err(|_| XSDError::CvcSimpleType)?;
                    return Ok(SchemaValue {
                        r#type: SchemaValueType::Time,
                        list: false,
                        raw_value: collapsed.into(),
                        canonic_value: dur.to_string().into(),
                    });
                }
                "date" => {
                    let dur = collapsed
                        .parse::<Date>()
                        .map_err(|_| XSDError::CvcSimpleType)?;
                    return Ok(SchemaValue {
                        r#type: SchemaValueType::Date,
                        list: false,
                        raw_value: collapsed.into(),
                        canonic_value: dur.to_string().into(),
                    });
                }
                "gYearMonth" => {
                    let dur = collapsed
                        .parse::<GYearMonth>()
                        .map_err(|_| XSDError::CvcSimpleType)?;
                    return Ok(SchemaValue {
                        r#type: SchemaValueType::GYearMonth,
                        list: false,
                        raw_value: collapsed.into(),
                        canonic_value: dur.to_string().into(),
                    });
                }
                "gYear" => {
                    let dur = collapsed
                        .parse::<GYear>()
                        .map_err(|_| XSDError::CvcSimpleType)?;
                    return Ok(SchemaValue {
                        r#type: SchemaValueType::GYear,
                        list: false,
                        raw_value: collapsed.into(),
                        canonic_value: dur.to_string().into(),
                    });
                }
                "gMonthDay" => {
                    let dur = collapsed
                        .parse::<GMonthDay>()
                        .map_err(|_| XSDError::CvcSimpleType)?;
                    return Ok(SchemaValue {
                        r#type: SchemaValueType::GMonthDay,
                        list: false,
                        raw_value: collapsed.into(),
                        canonic_value: dur.to_string().into(),
                    });
                }
                "gDay" => {
                    let dur = collapsed
                        .parse::<GDay>()
                        .map_err(|_| XSDError::CvcSimpleType)?;
                    return Ok(SchemaValue {
                        r#type: SchemaValueType::GDay,
                        list: false,
                        raw_value: collapsed.into(),
                        canonic_value: dur.to_string().into(),
                    });
                }
                "gMonth" => {
                    let dur = collapsed
                        .parse::<GMonth>()
                        .map_err(|_| XSDError::CvcSimpleType)?;
                    return Ok(SchemaValue {
                        r#type: SchemaValueType::GMonth,
                        list: false,
                        raw_value: collapsed.into(),
                        canonic_value: dur.to_string().into(),
                    });
                }
                "hexBinary" => {
                    if collapsed.len() % 2 != 0 || collapsed.bytes().any(|b| !b.is_ascii_hexdigit())
                    {
                        return Err(XMLError::XSDError(XSDError::CvcSimpleType));
                    }
                    let canonic_value = collapsed.to_ascii_lowercase().into();
                    return Ok(SchemaValue {
                        r#type: SchemaValueType::HexBinary,
                        list: false,
                        raw_value: collapsed.into(),
                        canonic_value,
                    });
                }
                "base64Binary" => {
                    let _ = Base64Binary::from_encoded(collapsed.bytes(), true)
                        .map_err(|_| XSDError::CvcSimpleType)?;
                    let raw_value: Box<str> = collapsed.into();
                    return Ok(SchemaValue {
                        r#type: SchemaValueType::Base64Binary,
                        list: false,
                        canonic_value: raw_value.clone(),
                        raw_value,
                    });
                }
                "anyURI" => {
                    let _ = URIString::parse(collapsed.as_ref())
                        .map_err(|_| XSDError::CvcSimpleType)?;
                    let raw_value: Box<str> = collapsed.into();
                    return Ok(SchemaValue {
                        r#type: SchemaValueType::AnyURI,
                        list: false,
                        canonic_value: raw_value.clone(),
                        raw_value,
                    });
                }
                ty @ ("QName" | "NOTATION") => {
                    if !XMLVersion::XML10.validate_qname(&collapsed) {
                        return Err(XMLError::XSDError(XSDError::CvcSimpleType));
                    }
                    let r#type = if ty == "QName" {
                        SchemaValueType::QName
                    } else {
                        SchemaValueType::Notation
                    };
                    // Although there is no canonical lexical representation for QName and
                    // NOTATION, for ease of comparison, use the format "{namespace}localname".
                    return if let Some((prefix, local)) = collapsed.split_once(':') {
                        let namespace = ns_context.get(prefix).ok_or(XSDError::CvcSimpleType)?;
                        Ok(SchemaValue {
                            r#type,
                            list: false,
                            canonic_value: format!("{{{}}}{local}", namespace.namespace_name)
                                .into(),
                            raw_value: collapsed.into(),
                        })
                    } else if let Some(namespace) = ns_context.get("") {
                        Ok(SchemaValue {
                            r#type,
                            list: false,
                            canonic_value: format!("{{{}}}{collapsed}", namespace.namespace_name)
                                .into(),
                            raw_value: collapsed.into(),
                        })
                    } else {
                        let raw_value: Box<str> = collapsed.into();
                        Ok(SchemaValue {
                            r#type,
                            list: false,
                            canonic_value: raw_value.clone(),
                            raw_value,
                        })
                    };
                }
                "normalizedString" => {
                    return if !value.contains(['\x0D', '\x0A', '\x09']) {
                        Ok(SchemaValue {
                            r#type: SchemaValueType::String,
                            list: false,
                            raw_value: value.into(),
                            canonic_value: value.into(),
                        })
                    } else {
                        Err(XMLError::XSDError(XSDError::CvcSimpleType))
                    };
                }
                "token" => {
                    return if !value.starts_with('\x20')
                        && !value.ends_with('\x20')
                        && !value.contains(['\x0D', '\x0A', '\x09'])
                        && !value.contains("\x20\x20")
                    {
                        Ok(SchemaValue {
                            r#type: SchemaValueType::String,
                            list: false,
                            raw_value: value.into(),
                            canonic_value: value.into(),
                        })
                    } else {
                        Err(XMLError::XSDError(XSDError::CvcSimpleType))
                    };
                }
                "language" => {
                    let mut split = value.split('-');
                    return if split.next().is_some_and(|s| {
                        (1..=8).contains(&s.len()) && s.bytes().all(|b| b.is_ascii_alphabetic())
                    }) && split.all(|s| {
                        (1..=8).contains(&s.len()) && s.bytes().all(|b| b.is_ascii_alphanumeric())
                    }) {
                        Ok(SchemaValue {
                            r#type: SchemaValueType::String,
                            list: false,
                            raw_value: value.into(),
                            canonic_value: value.into(),
                        })
                    } else {
                        Err(XMLError::XSDError(XSDError::CvcSimpleType))
                    };
                }
                "NMTOKEN" => {
                    return if XMLVersion::default().validate_nmtoken(value) {
                        Ok(SchemaValue {
                            r#type: SchemaValueType::String,
                            list: false,
                            raw_value: value.into(),
                            canonic_value: value.into(),
                        })
                    } else {
                        Err(XMLError::XSDError(XSDError::CvcSimpleType))
                    };
                }
                "NMTOKENS" => {
                    return if XMLVersion::default().validate_nmtokens(&collapsed) {
                        Ok(SchemaValue {
                            r#type: SchemaValueType::String,
                            list: true,
                            raw_value: collapsed.as_ref().into(),
                            canonic_value: collapsed.into(),
                        })
                    } else {
                        Err(XMLError::XSDError(XSDError::CvcSimpleType))
                    };
                }
                "Name" => {
                    return if XMLVersion::default().validate_name(value) {
                        Ok(SchemaValue {
                            r#type: SchemaValueType::String,
                            list: false,
                            raw_value: value.into(),
                            canonic_value: value.into(),
                        })
                    } else {
                        Err(XMLError::XSDError(XSDError::CvcSimpleType))
                    };
                }
                "NCName" | "ID" | "IDREF" | "ENTITY" => {
                    return if XMLVersion::default().validate_ncname(value) {
                        Ok(SchemaValue {
                            r#type: SchemaValueType::String,
                            list: false,
                            raw_value: value.into(),
                            canonic_value: value.into(),
                        })
                    } else {
                        Err(XMLError::XSDError(XSDError::CvcSimpleType))
                    };
                }
                "IDREFS" | "ENTITIES" => {
                    return if collapsed
                        .split('\x20')
                        .any(|tk| !XMLVersion::XML10.validate_ncname(tk))
                    {
                        Err(XMLError::XSDError(XSDError::CvcSimpleType))
                    } else {
                        Ok(SchemaValue {
                            r#type: SchemaValueType::String,
                            list: true,
                            raw_value: collapsed.as_ref().into(),
                            canonic_value: collapsed.into(),
                        })
                    };
                }
                "integer" => {
                    let mut canonic = String::with_capacity(collapsed.len());
                    let mut cur = collapsed.as_ref();
                    if let Some(rem) = cur.strip_prefix('-') {
                        cur = rem;
                        canonic.push('-');
                    } else if let Some(rem) = cur.strip_prefix('+') {
                        cur = rem;
                    }
                    cur = cur.trim_start_matches('0');
                    if cur.is_empty() {
                        canonic.clear();
                        canonic.push('0');
                    } else {
                        canonic.push_str(cur);
                    }
                    cur = cur.trim_start_matches(|c: char| c.is_ascii_digit());
                    if !cur.is_empty() {
                        return Err(XMLError::XSDError(XSDError::CvcSimpleType));
                    }
                    return Ok(SchemaValue {
                        r#type: SchemaValueType::Decimal,
                        list: false,
                        raw_value: collapsed.into(),
                        canonic_value: canonic.into(),
                    });
                }
                "nonPositiveInteger" => {
                    let value = self.parse_simple_type(
                        &collapsed,
                        XSD_INTEGER.target_namespace().as_deref(),
                        &XSD_INTEGER.name(),
                        ns_context,
                    )?;
                    if !value.canonic_value.starts_with('-') && &*value.canonic_value != "0" {
                        return Err(XMLError::XSDError(XSDError::CvcSimpleType));
                    }
                    return Ok(value);
                }
                "negativeInteger" => {
                    let value = self.parse_simple_type(
                        &collapsed,
                        XSD_NONPOSITIVE_INTEGER.target_namespace().as_deref(),
                        &XSD_NONPOSITIVE_INTEGER.name(),
                        ns_context,
                    )?;
                    if &*value.canonic_value == "0" {
                        return Err(XMLError::XSDError(XSDError::CvcSimpleType));
                    }
                    return Ok(value);
                }
                "long" => {
                    let val = collapsed
                        .parse::<i64>()
                        .map_err(|_| XSDError::CvcSimpleType)?;
                    return Ok(SchemaValue {
                        r#type: SchemaValueType::Decimal,
                        list: false,
                        raw_value: collapsed.into(),
                        canonic_value: val.to_string().into(),
                    });
                }
                "int" => {
                    let val = collapsed
                        .parse::<i32>()
                        .map_err(|_| XSDError::CvcSimpleType)?;
                    return Ok(SchemaValue {
                        r#type: SchemaValueType::Decimal,
                        list: false,
                        raw_value: collapsed.into(),
                        canonic_value: val.to_string().into(),
                    });
                }
                "short" => {
                    let val = collapsed
                        .parse::<i16>()
                        .map_err(|_| XSDError::CvcSimpleType)?;
                    return Ok(SchemaValue {
                        r#type: SchemaValueType::Decimal,
                        list: false,
                        raw_value: collapsed.into(),
                        canonic_value: val.to_string().into(),
                    });
                }
                "byte" => {
                    let val = collapsed
                        .parse::<i8>()
                        .map_err(|_| XSDError::CvcSimpleType)?;
                    return Ok(SchemaValue {
                        r#type: SchemaValueType::Decimal,
                        list: false,
                        raw_value: collapsed.into(),
                        canonic_value: val.to_string().into(),
                    });
                }
                "nonNegativeInteger" => {
                    let value = self.parse_simple_type(
                        &collapsed,
                        XSD_INTEGER.target_namespace().as_deref(),
                        &XSD_INTEGER.name(),
                        ns_context,
                    )?;
                    if value.canonic_value.starts_with('-') {
                        return Err(XMLError::XSDError(XSDError::CvcSimpleType));
                    }
                    return Ok(value);
                }
                "unsignedLong" => {
                    let val = collapsed
                        .parse::<u64>()
                        .map_err(|_| XSDError::CvcSimpleType)?;
                    return Ok(SchemaValue {
                        r#type: SchemaValueType::Decimal,
                        list: false,
                        raw_value: collapsed.into(),
                        canonic_value: val.to_string().into(),
                    });
                }
                "unsignedInt" => {
                    let val = collapsed
                        .parse::<u32>()
                        .map_err(|_| XSDError::CvcSimpleType)?;
                    return Ok(SchemaValue {
                        r#type: SchemaValueType::Decimal,
                        list: false,
                        raw_value: collapsed.into(),
                        canonic_value: val.to_string().into(),
                    });
                }
                "unsignedShort" => {
                    let val = collapsed
                        .parse::<u16>()
                        .map_err(|_| XSDError::CvcSimpleType)?;
                    return Ok(SchemaValue {
                        r#type: SchemaValueType::Decimal,
                        list: false,
                        raw_value: collapsed.into(),
                        canonic_value: val.to_string().into(),
                    });
                }
                "unsignedByte" => {
                    let val = collapsed
                        .parse::<u8>()
                        .map_err(|_| XSDError::CvcSimpleType)?;
                    return Ok(SchemaValue {
                        r#type: SchemaValueType::Decimal,
                        list: false,
                        raw_value: collapsed.into(),
                        canonic_value: val.to_string().into(),
                    });
                }
                "positiveInteger" => {
                    let value = self.parse_simple_type(
                        &collapsed,
                        XSD_NONNEGATIVE_INTEGER.target_namespace().as_deref(),
                        &XSD_NONNEGATIVE_INTEGER.name(),
                        ns_context,
                    )?;
                    if &*value.canonic_value == "0" {
                        return Err(XMLError::XSDError(XSDError::CvcSimpleType));
                    }
                    return Ok(value);
                }
                _ => unreachable!(),
            }
        }

        let t = self
            .type_defs
            .get(&(target_namespace.map(From::from), name.into()))
            .and_then(|t| t.as_simple_type())
            .ok_or(XSDError::MissingSubComponent)?;
        assert!(!t.is_builtin_type());
        let value = normalize_whitespace(
            value,
            t.facets
                .whitespace
                .map(|w| w.value)
                .unwrap_or(Whitespace::Preserve),
        );
        let value = if t.base_type_definition.0.as_deref() == Some(XML_SCHEMA_NAMESPACE)
            && t.base_type_definition.1.as_ref() == "anySimpleType"
        {
            match t.variety.as_ref() {
                Some(Variety::List(ns, name)) => {
                    let collapsed = normalize_whitespace(&value, Whitespace::Collapse);
                    let mut canonic = String::with_capacity(collapsed.len());
                    let mut r#type = SchemaValueType::String;
                    for (i, s) in collapsed.split('\x20').enumerate() {
                        let value = self.parse_simple_type(s, ns.as_deref(), name, ns_context)?;
                        if i > 0 {
                            canonic.push('\x20');
                        }
                        r#type = value.r#type;
                        canonic.push_str(&value.canonic_value);
                    }
                    SchemaValue {
                        r#type,
                        list: true,
                        raw_value: collapsed.into(),
                        canonic_value: canonic.into(),
                    }
                }
                Some(Variety::Union(members)) => {
                    let mut ret = Err(XMLError::XSDError(XSDError::MissingSubComponent));
                    for (ns, name) in members.iter() {
                        if let Ok(value) =
                            self.parse_simple_type(&value, ns.as_deref(), name, ns_context)
                        {
                            ret = Ok(value);
                            break;
                        }
                    }
                    ret?
                }
                _ => unreachable!(),
            }
        } else {
            self.parse_simple_type(
                &value,
                t.base_type_definition.0.as_deref(),
                &t.base_type_definition.1,
                ns_context,
            )?
        };

        let length = if value.list {
            value.raw_value.split('\x20').count()
        } else {
            match value.r#type {
                SchemaValueType::String | SchemaValueType::AnyURI => value.raw_value.len(),
                SchemaValueType::HexBinary => value.raw_value.len() / 2,
                SchemaValueType::Base64Binary => {
                    Base64Binary::from_encoded(value.raw_value.bytes(), true)
                        .map_err(|_| XSDError::CvcSimpleType)?
                        .decode()
                        .count()
                }
                _ => 0,
            }
        };

        if t.facets.length.is_some_and(|l| length != l.value) {
            // [cvc-length-valid]
            return Err(XMLError::XSDError(XSDError::CvcLengthValid));
        }
        if t.facets.min_length.is_some_and(|l| length < l.value) {
            // [cvc-minLength-valid]
            return Err(XMLError::XSDError(XSDError::CvcMinLengthValid));
        }
        if t.facets.max_length.is_some_and(|l| l.value < length) {
            // [cvc-maxLength-valid]
            return Err(XMLError::XSDError(XSDError::CvcMaxLengthValid));
        }
        if !t.facets.pattern.is_empty() {
            // [cvc-pattern-valid]
            let mut valid = false;
            for pattern in &t.facets.pattern {
                let regex =
                    XSRegexp::compile(&pattern.value).map_err(|_| XSDError::CvcPatternValid)?;
                if regex.is_match(&value.raw_value) {
                    valid = true;
                    break;
                }
            }
            if !valid {
                return Err(XMLError::XSDError(XSDError::CvcPatternValid));
            }
        }
        if !t.facets.enumeration.is_empty() {
            // [cvc-enumeration-valid]
            let mut valid = false;
            for enumeration in &t.facets.enumeration {
                if enumeration.value.canonic_value == value.canonic_value {
                    valid = true;
                    break;
                }
            }
            if !valid {
                return Err(XMLError::XSDError(XSDError::CvcEnumerationValid));
            }
        }
        if let Some(max) = t.facets.max_inclusive.as_ref() {
            // [cvc-maxInclusive-valid]
            if value > max.value {
                return Err(XMLError::XSDError(XSDError::CvcMaxInclusiveValid));
            }
        }
        if let Some(max) = t.facets.max_exclusive.as_ref() {
            // [cvc-maxExclusive-valid]
            if value >= max.value {
                return Err(XMLError::XSDError(XSDError::CvcMaxExclusiveValid));
            }
        }
        if let Some(min) = t.facets.min_exclusive.as_ref() {
            // [cvc-minExclusive-valid]
            if value <= min.value {
                return Err(XMLError::XSDError(XSDError::CvcMinExclusiveValid));
            }
        }
        if let Some(min) = t.facets.min_inclusive.as_ref() {
            // [cvc-minInclusive-valid]
            if value < min.value {
                return Err(XMLError::XSDError(XSDError::CvcMinInclusiveValid));
            }
        }
        if let Some(l) = t.facets.total_digits {
            // [cvc-totalDigits-valid]
            let total_digits = match value.canonic_value.split_once('.') {
                Some(("0", "0")) => 1,
                Some(("0", i)) => i.len(),
                Some((i, "0")) => i.trim_start_matches('-').len(),
                Some((i, f)) => i.trim_start_matches('-').len() + f.len(),
                None => value.canonic_value.trim_start_matches('-').len(),
            };
            if total_digits > l.value {
                return Err(XMLError::XSDError(XSDError::CvcTotalDigitsValid));
            }
        }
        if let Some(l) = t.facets.fraction_digits {
            // [cvc-fractionDigits-valid]
            let fraction_digits = match value.canonic_value.split_once('.') {
                Some((_, i)) => i.trim_end_matches('0').len(),
                None => 0,
            };
            if fraction_digits > l.value {
                return Err(XMLError::XSDError(XSDError::CvcFractionDigitsValid));
            }
        }
        Ok(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SchemaValueType {
    /// String type value.
    String,
    /// Boolean type value.
    Boolean,
    /// Decimal type value.
    Decimal,
    /// Float type value.
    Float,
    /// Double type value.
    Double,
    /// Duration type value.
    Duration,
    /// DateTime type value.
    DateTime,
    /// Time type value.
    Time,
    /// Date type value.
    Date,
    /// gYearMonth type value.
    GYearMonth,
    /// gYear type value.
    GYear,
    /// gMonthDay type value.
    GMonthDay,
    /// gDay type value.
    GDay,
    /// gMonth type value.
    GMonth,
    /// Hex binary type value.
    HexBinary,
    /// Base64 binary type value.
    Base64Binary,
    /// anyURI type value.
    AnyURI,
    /// QName type value.
    QName,
    /// Notation type value.
    Notation,
}

#[derive(Debug, Clone)]
pub struct SchemaValue {
    pub(super) r#type: SchemaValueType,
    pub(super) list: bool,
    pub(super) raw_value: Box<str>,
    pub(super) canonic_value: Box<str>,
}

impl PartialEq for SchemaValue {
    fn eq(&self, other: &Self) -> bool {
        self.r#type == other.r#type
            && self.list == other.list
            && self.canonic_value == other.canonic_value
    }
}

impl PartialOrd for SchemaValue {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.list || other.list || self.r#type != other.r#type {
            return None;
        }

        match self.r#type {
            SchemaValueType::Decimal => {
                let (lhi, llo) = self.canonic_value.split_once('.')?;
                let (rhi, rlo) = other.canonic_value.split_once('.')?;
                let ml = lhi.starts_with('-');
                let mr = rhi.starts_with('-');
                if ml != mr {
                    return mr.partial_cmp(&ml);
                }

                if lhi.len() != rhi.len() {
                    if ml {
                        Some(rhi.len().cmp(&lhi.len()))
                    } else {
                        Some(lhi.len().cmp(&rhi.len()))
                    }
                } else {
                    match lhi.cmp(rhi) {
                        ret @ (std::cmp::Ordering::Less | std::cmp::Ordering::Greater) => {
                            Some(if ml { ret.reverse() } else { ret })
                        }
                        std::cmp::Ordering::Equal => {
                            let ret = llo.cmp(rlo);
                            Some(if ml { ret.reverse() } else { ret })
                        }
                    }
                }
            }
            SchemaValueType::Float => self
                .canonic_value
                .parse::<f32>()
                .ok()?
                .partial_cmp(&other.canonic_value.parse::<f32>().ok()?),
            SchemaValueType::Double => self
                .canonic_value
                .parse::<f64>()
                .ok()?
                .partial_cmp(&other.canonic_value.parse::<f64>().ok()?),
            SchemaValueType::Duration => self
                .canonic_value
                .parse::<Duration>()
                .ok()?
                .partial_cmp(&other.canonic_value.parse::<Duration>().ok()?),
            SchemaValueType::DateTime => self
                .canonic_value
                .parse::<DateTime>()
                .ok()?
                .partial_cmp(&other.canonic_value.parse::<DateTime>().ok()?),
            SchemaValueType::Time => self
                .canonic_value
                .parse::<Time>()
                .ok()?
                .partial_cmp(&other.canonic_value.parse::<Time>().ok()?),
            SchemaValueType::Date => self
                .canonic_value
                .parse::<Date>()
                .ok()?
                .partial_cmp(&other.canonic_value.parse::<Date>().ok()?),
            SchemaValueType::GYearMonth => self
                .canonic_value
                .parse::<GYearMonth>()
                .ok()?
                .partial_cmp(&other.canonic_value.parse::<GYearMonth>().ok()?),
            SchemaValueType::GYear => self
                .canonic_value
                .parse::<GYear>()
                .ok()?
                .partial_cmp(&other.canonic_value.parse::<GYear>().ok()?),
            SchemaValueType::GMonthDay => self
                .canonic_value
                .parse::<GMonthDay>()
                .ok()?
                .partial_cmp(&other.canonic_value.parse::<GMonthDay>().ok()?),
            SchemaValueType::GDay => self
                .canonic_value
                .parse::<GDay>()
                .ok()?
                .partial_cmp(&other.canonic_value.parse::<GDay>().ok()?),
            SchemaValueType::GMonth => self
                .canonic_value
                .parse::<GMonth>()
                .ok()?
                .partial_cmp(&other.canonic_value.parse::<GMonth>().ok()?),
            _ => None,
        }
    }
}

impl Default for SchemaValue {
    fn default() -> Self {
        Self {
            r#type: SchemaValueType::String,
            list: false,
            raw_value: Default::default(),
            canonic_value: Default::default(),
        }
    }
}

/// # Reference
/// - [4.3.6 whiteSpace](https://www.w3.org/TR/xmlschema-2/#rf-whiteSpace)
fn normalize_whitespace(value: &str, whitespace: Whitespace) -> Cow<'_, str> {
    match whitespace {
        Whitespace::Preserve => Cow::Borrowed(value),
        Whitespace::Replace => {
            if value.contains(['\x09', '\x0A', '\x0D']) {
                Cow::Owned(value.replace(['\x09', '\x0A', '\x0D'], "\x20"))
            } else {
                Cow::Borrowed(value)
            }
        }
        Whitespace::Collapse => {
            let value = value.trim_matches(|c: char| XMLVersion::XML10.is_whitespace(c));
            if value.contains(['\x09', '\x0A', '\x0D']) || value.contains("\x20\x20") {
                let mut ret = String::with_capacity(value.len());
                for seg in value.split(|c: char| XMLVersion::XML10.is_whitespace(c)) {
                    if !ret.is_empty() {
                        ret.push('\x20');
                    }
                    ret.push_str(seg);
                }
                Cow::Owned(ret)
            } else {
                Cow::Borrowed(value)
            }
        }
    }
}
