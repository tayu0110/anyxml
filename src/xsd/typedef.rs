use std::{
    num::NonZeroUsize,
    sync::{Arc, LazyLock},
};

use crate::{
    automata::xsregexp::XSRegexp,
    xsd::{XML_SCHEMA_NAMESPACE, XSDError, parse::TypeDefinition},
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
    pub fn length(&self) -> Option<usize> {
        self.facets.length.map(|l| l.value)
    }

    pub fn set_length(&mut self, length: usize, fixed: bool) -> Result<(), XSDError> {
        if self.facets.length.is_some() {
            return Err(XSDError::SrcSingleFacetValue);
        }
        match self.variety.as_ref() {
            Some(Variety::Union(_)) => return Err(XSDError::CosApplicableFacets),
            Some(Variety::Atomic(_, n))
                if !matches!(
                    n.as_ref(),
                    "string" | "hexBinary" | "base64Binary" | "anyURI" | "QName" | "NOTATION"
                ) =>
            {
                return Err(XSDError::CosApplicableFacets);
            }
            _ => {}
        }
        self.facets.length = Some(Length {
            value: length,
            fixed,
        });
        Ok(())
    }

    pub fn min_length(&self) -> Option<usize> {
        self.facets.min_length.map(|l| l.value)
    }

    pub fn set_min_length(&mut self, length: usize, fixed: bool) -> Result<(), XSDError> {
        if self.facets.min_length.is_some() {
            return Err(XSDError::SrcSingleFacetValue);
        }
        match self.variety.as_ref() {
            Some(Variety::Union(_)) => return Err(XSDError::CosApplicableFacets),
            Some(Variety::Atomic(_, n))
                if !matches!(
                    n.as_ref(),
                    "string" | "hexBinary" | "base64Binary" | "anyURI" | "QName" | "NOTATION"
                ) =>
            {
                return Err(XSDError::CosApplicableFacets);
            }
            _ => {}
        }
        self.facets.min_length = Some(Length {
            value: length,
            fixed,
        });
        Ok(())
    }

    pub fn max_length(&self) -> Option<usize> {
        self.facets.max_length.map(|l| l.value)
    }

    pub fn set_max_length(&mut self, length: usize, fixed: bool) -> Result<(), XSDError> {
        if self.facets.max_length.is_some() {
            return Err(XSDError::SrcSingleFacetValue);
        }
        match self.variety.as_ref() {
            Some(Variety::Union(_)) => return Err(XSDError::CosApplicableFacets),
            Some(Variety::Atomic(_, n))
                if !matches!(
                    n.as_ref(),
                    "string" | "hexBinary" | "base64Binary" | "anyURI" | "QName" | "NOTATION"
                ) =>
            {
                return Err(XSDError::CosApplicableFacets);
            }
            _ => {}
        }
        self.facets.max_length = Some(Length {
            value: length,
            fixed,
        });
        Ok(())
    }

    pub fn set_pattern(&mut self, pattern: impl Into<String>) -> Result<(), XSDError> {
        let pattern: String = pattern.into();
        if let Err(err) = XSRegexp::compile(&pattern) {
            return Err(XSDError::RegexpError(err));
        }
        self.facets.pattern.push(Pattern { value: pattern });
        Ok(())
    }

    pub fn set_enumeration(&mut self, value: impl Into<String>) -> Result<(), XSDError> {
        match self.variety.as_ref() {
            Some(Variety::Atomic(_, n)) if n.as_ref() == "boolean" => {
                return Err(XSDError::CosApplicableFacets);
            }
            _ => {}
        }
        self.facets.enumeration.push(Enumeration {
            value: value.into(),
        });
        Ok(())
    }

    pub fn whitespace(&self) -> Option<Whitespace> {
        self.facets.whitespace.map(|w| w.value)
    }

    pub fn set_whitespace(&mut self, value: Whitespace, fixed: bool) -> Result<(), XSDError> {
        if self.facets.whitespace.is_some() {
            return Err(XSDError::SrcSingleFacetValue);
        }
        if matches!(self.variety, Some(Variety::Union(_))) {
            return Err(XSDError::CosApplicableFacets);
        }
        self.facets.whitespace = Some(WhitespaceConstraint { value, fixed });
        Ok(())
    }

    pub fn set_max_inclusive(
        &mut self,
        value: impl Into<String>,
        fixed: bool,
    ) -> Result<(), XSDError> {
        if self.facets.max_inclusive.is_some() {
            return Err(XSDError::SrcSingleFacetValue);
        }
        match self.variety.as_ref() {
            Some(Variety::List(_, _) | Variety::Union(_)) => {
                return Err(XSDError::CosApplicableFacets);
            }
            Some(Variety::Atomic(_, n))
                if !matches!(
                    n.as_ref(),
                    "float"
                        | "double"
                        | "decimal"
                        | "duration"
                        | "dateTime"
                        | "time"
                        | "date"
                        | "gYearMonth"
                        | "gYear"
                        | "gMonthDay"
                        | "gDay"
                        | "gMonth"
                ) =>
            {
                return Err(XSDError::CosApplicableFacets);
            }
            _ => {}
        }
        self.facets.max_inclusive = Some(Boundary {
            value: value.into(),
            fixed,
        });
        Ok(())
    }

    pub fn set_max_exclusive(
        &mut self,
        value: impl Into<String>,
        fixed: bool,
    ) -> Result<(), XSDError> {
        if self.facets.max_exclusive.is_some() {
            return Err(XSDError::SrcSingleFacetValue);
        }
        match self.variety.as_ref() {
            Some(Variety::List(_, _) | Variety::Union(_)) => {
                return Err(XSDError::CosApplicableFacets);
            }
            Some(Variety::Atomic(_, n))
                if !matches!(
                    n.as_ref(),
                    "float"
                        | "double"
                        | "decimal"
                        | "duration"
                        | "dateTime"
                        | "time"
                        | "date"
                        | "gYearMonth"
                        | "gYear"
                        | "gMonthDay"
                        | "gDay"
                        | "gMonth"
                ) =>
            {
                return Err(XSDError::CosApplicableFacets);
            }
            _ => {}
        }
        self.facets.max_exclusive = Some(Boundary {
            value: value.into(),
            fixed,
        });
        Ok(())
    }

    pub fn set_min_exclusive(
        &mut self,
        value: impl Into<String>,
        fixed: bool,
    ) -> Result<(), XSDError> {
        if self.facets.min_exclusive.is_some() {
            return Err(XSDError::SrcSingleFacetValue);
        }
        match self.variety.as_ref() {
            Some(Variety::List(_, _) | Variety::Union(_)) => {
                return Err(XSDError::CosApplicableFacets);
            }
            Some(Variety::Atomic(_, n))
                if !matches!(
                    n.as_ref(),
                    "float"
                        | "double"
                        | "decimal"
                        | "duration"
                        | "dateTime"
                        | "time"
                        | "date"
                        | "gYearMonth"
                        | "gYear"
                        | "gMonthDay"
                        | "gDay"
                        | "gMonth"
                ) =>
            {
                return Err(XSDError::CosApplicableFacets);
            }
            _ => {}
        }
        self.facets.min_exclusive = Some(Boundary {
            value: value.into(),
            fixed,
        });
        Ok(())
    }

    pub fn set_min_inclusive(
        &mut self,
        value: impl Into<String>,
        fixed: bool,
    ) -> Result<(), XSDError> {
        if self.facets.min_inclusive.is_some() {
            return Err(XSDError::SrcSingleFacetValue);
        }
        match self.variety.as_ref() {
            Some(Variety::List(_, _) | Variety::Union(_)) => {
                return Err(XSDError::CosApplicableFacets);
            }
            Some(Variety::Atomic(_, n))
                if !matches!(
                    n.as_ref(),
                    "float"
                        | "double"
                        | "decimal"
                        | "duration"
                        | "dateTime"
                        | "time"
                        | "date"
                        | "gYearMonth"
                        | "gYear"
                        | "gMonthDay"
                        | "gDay"
                        | "gMonth"
                ) =>
            {
                return Err(XSDError::CosApplicableFacets);
            }
            _ => {}
        }
        self.facets.min_inclusive = Some(Boundary {
            value: value.into(),
            fixed,
        });
        Ok(())
    }

    pub fn set_total_digits(&mut self, value: NonZeroUsize, fixed: bool) -> Result<(), XSDError> {
        if self.facets.total_digits.is_some() {
            return Err(XSDError::SrcSingleFacetValue);
        }
        match self.variety.as_ref() {
            Some(Variety::List(_, _) | Variety::Union(_)) => {
                return Err(XSDError::CosApplicableFacets);
            }
            Some(Variety::Atomic(_, n)) if !matches!(n.as_ref(), "decimal") => {
                return Err(XSDError::CosApplicableFacets);
            }
            _ => {}
        }
        self.facets.total_digits = Some(Length {
            value: value.get(),
            fixed,
        });
        Ok(())
    }

    pub fn set_fraction_digits(&mut self, value: usize, fixed: bool) -> Result<(), XSDError> {
        if self.facets.fraction_digits.is_some() {
            return Err(XSDError::SrcSingleFacetValue);
        }
        match self.variety.as_ref() {
            Some(Variety::List(_, _) | Variety::Union(_)) => {
                return Err(XSDError::CosApplicableFacets);
            }
            Some(Variety::Atomic(_, n)) if !matches!(n.as_ref(), "decimal") => {
                return Err(XSDError::CosApplicableFacets);
            }
            _ => {}
        }
        self.facets.fraction_digits = Some(Length { value, fixed });
        Ok(())
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
    pub(super) value: String,
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

#[derive(Clone, Default)]
pub(super) struct Boundary {
    pub(super) value: String,
    pub(super) fixed: bool,
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
    Atomic(Option<Arc<str>>, Arc<str>),
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
        variety: Some(Variety::Atomic(
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            name,
        )),
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
        variety: Some(Variety::Atomic(
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            name,
        )),
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
        variety: Some(Variety::Atomic(
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            name,
        )),
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
        variety: Some(Variety::Atomic(
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            name,
        )),
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
        variety: Some(Variety::Atomic(
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            name,
        )),
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
        variety: Some(Variety::Atomic(
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            name,
        )),
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
        variety: Some(Variety::Atomic(
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            name,
        )),
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
        variety: Some(Variety::Atomic(
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            name,
        )),
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
        variety: Some(Variety::Atomic(
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            name,
        )),
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
        variety: Some(Variety::Atomic(
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            name,
        )),
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
        variety: Some(Variety::Atomic(
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            name,
        )),
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
        variety: Some(Variety::Atomic(
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            name,
        )),
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
        variety: Some(Variety::Atomic(
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            name,
        )),
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
        variety: Some(Variety::Atomic(
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            name,
        )),
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
        variety: Some(Variety::Atomic(
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            name,
        )),
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
        variety: Some(Variety::Atomic(
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            name,
        )),
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
        variety: Some(Variety::Atomic(
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            name,
        )),
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
        variety: Some(Variety::Atomic(
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            name,
        )),
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
        variety: Some(Variety::Atomic(
            XSD_ANY_SIMPLE_TYPE.target_namespace(),
            name,
        )),
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
        variety: Some(Variety::Atomic(
            XSD_STRING.target_namespace(),
            XSD_STRING.name(),
        )),
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
        variety: Some(Variety::Atomic(
            XSD_STRING.target_namespace(),
            XSD_STRING.name(),
        )),
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
        variety: Some(Variety::Atomic(
            XSD_STRING.target_namespace(),
            XSD_STRING.name(),
        )),
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
        variety: Some(Variety::Atomic(
            XSD_STRING.target_namespace(),
            XSD_STRING.name(),
        )),
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
        variety: Some(Variety::Atomic(
            XSD_STRING.target_namespace(),
            XSD_STRING.name(),
        )),
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
        variety: Some(Variety::Atomic(
            XSD_STRING.target_namespace(),
            XSD_STRING.name(),
        )),
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
        variety: Some(Variety::Atomic(
            XSD_STRING.target_namespace(),
            XSD_STRING.name(),
        )),
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
        variety: Some(Variety::Atomic(
            XSD_STRING.target_namespace(),
            XSD_STRING.name(),
        )),
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
        variety: Some(Variety::Atomic(
            XSD_STRING.target_namespace(),
            XSD_STRING.name(),
        )),
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
        variety: Some(Variety::Atomic(
            XSD_DECIMAL.target_namespace(),
            XSD_DECIMAL.name(),
        )),
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
        variety: Some(Variety::Atomic(
            XSD_DECIMAL.target_namespace(),
            XSD_DECIMAL.name(),
        )),
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
        variety: Some(Variety::Atomic(
            XSD_DECIMAL.target_namespace(),
            XSD_DECIMAL.name(),
        )),
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
        variety: Some(Variety::Atomic(
            XSD_DECIMAL.target_namespace(),
            XSD_DECIMAL.name(),
        )),
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
        variety: Some(Variety::Atomic(
            XSD_DECIMAL.target_namespace(),
            XSD_DECIMAL.name(),
        )),
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
        variety: Some(Variety::Atomic(
            XSD_DECIMAL.target_namespace(),
            XSD_DECIMAL.name(),
        )),
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
        variety: Some(Variety::Atomic(
            XSD_DECIMAL.target_namespace(),
            XSD_DECIMAL.name(),
        )),
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
        variety: Some(Variety::Atomic(
            XSD_DECIMAL.target_namespace(),
            XSD_DECIMAL.name(),
        )),
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
        variety: Some(Variety::Atomic(
            XSD_DECIMAL.target_namespace(),
            XSD_DECIMAL.name(),
        )),
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
        variety: Some(Variety::Atomic(
            XSD_DECIMAL.target_namespace(),
            XSD_DECIMAL.name(),
        )),
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
        variety: Some(Variety::Atomic(
            XSD_DECIMAL.target_namespace(),
            XSD_DECIMAL.name(),
        )),
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
        variety: Some(Variety::Atomic(
            XSD_DECIMAL.target_namespace(),
            XSD_DECIMAL.name(),
        )),
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
        variety: Some(Variety::Atomic(
            XSD_DECIMAL.target_namespace(),
            XSD_DECIMAL.name(),
        )),
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
