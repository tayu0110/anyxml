//! Datatypes defined in [XML Schema Part 2: Datatypes Second Edition](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/).
//!
//! # Reference
//! - [XML Schema Part 2: Datatypes Second Edition](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/)

mod builtin;
mod facets;
mod value;

use std::{borrow::Cow, num::NonZeroUsize, sync::Arc};

use crate::{
    XMLVersion, automata::xsregexp::XSRegexp, base64::Base64Binary, datetime::*,
    sax::NamespaceStack, uri::URIString,
};

pub use builtin::*;
pub use facets::*;
pub use value::*;

/// XML Schema namespace
///
/// # Reference
/// - [3.1 Namespace considerations](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#namespaces)
pub const XML_SCHEMA_NAMESPACE: &str = "http://www.w3.org/2001/XMLSchema";
/// XML Schema datatype alternative namespace
///
/// # Reference
/// - [3.1 Namespace considerations](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#namespaces)
pub const XML_SCHEMA_DATATYPES_NAMESPACE: &str = "http://www.w3.org/2001/XMLSchema-datatypes";

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SchemaTypeError {
    DerivationFromAnySimpleType,
    DerivationByListFromList,
    DerivationPreventedByFinalization,
    FacetError(FacetError),
    ParseError(ParseError),
}

impl From<FacetError> for SchemaTypeError {
    fn from(value: FacetError) -> Self {
        Self::FacetError(value)
    }
}

impl From<ParseError> for SchemaTypeError {
    fn from(value: ParseError) -> Self {
        Self::ParseError(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParseError {
    AnySimpleType,
    Boolean,
    Decimal,
    Float,
    Double,
    Duration,
    DateTime,
    Time,
    Date,
    GYearMonth,
    GYear,
    GMonthDay,
    GDay,
    GMonth,
    HexBinary,
    Base64Binary,
    URI,
    QName,
    NOTATION,
    NormalizedString,
    Token,
    Language,
    NMTOKEN,
    NMTOKENS,
    Name,
    NCName,
    ID,
    IDREF,
    ENTITY,
    Integer,
    NonPositiveInteger,
    NegativeInteger,
    Long,
    Int,
    Short,
    Byte,
    NonNegativeInteger,
    UnsignedLong,
    UnsignedInt,
    UnsignedShort,
    UnsignedByte,
    PositiveInteger,
    Facet,
}

/// # Reference
/// - [4.1.1 The Simple Type Definition Schema Component](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#dc-defn)
#[allow(clippy::large_enum_variant)]
#[derive(Clone, PartialEq)]
pub enum SimpleTypeDefinition {
    AnySimpleType,
    BuiltinPrimitive(BuiltinPrimitiveType),
    BuiltinDerived(BuiltinDerivedType),
    UserDefined(UserDefinedType),
}

impl SimpleTypeDefinition {
    pub fn derive_by_restriction(
        self: Arc<Self>,
    ) -> Result<SimpleTypeDefinitionBuilder, SchemaTypeError> {
        if matches!(*self, SimpleTypeDefinition::AnySimpleType) {
            return Err(SchemaTypeError::DerivationFromAnySimpleType);
        }
        if self.r#final().restriction() {
            return Err(SchemaTypeError::DerivationPreventedByFinalization);
        }
        let variety = self.variety().unwrap();
        Ok(Self::derive(self, variety))
    }

    pub fn derive_by_list(self: Arc<Self>) -> Result<SimpleTypeDefinitionBuilder, SchemaTypeError> {
        if matches!(*self, SimpleTypeDefinition::AnySimpleType) {
            return Err(SchemaTypeError::DerivationFromAnySimpleType);
        }
        if matches!(self.variety(), Some(Variety::List(_))) {
            return Err(SchemaTypeError::DerivationByListFromList);
        }
        if self.r#final().list() {
            return Err(SchemaTypeError::DerivationPreventedByFinalization);
        }
        let variety = Variety::List(self.clone());
        Ok(Self::derive(
            SCHEMA_BUILTIN_ANY_SIMPLE_TYPE.clone(),
            variety,
        ))
    }

    pub fn derive_by_union(
        member: impl IntoIterator<Item = Arc<Self>>,
    ) -> Result<SimpleTypeDefinitionBuilder, SchemaTypeError> {
        let member = member.into_iter().collect::<Arc<[Arc<Self>]>>();
        if member
            .iter()
            .any(|m| matches!(**m, SimpleTypeDefinition::AnySimpleType))
        {
            return Err(SchemaTypeError::DerivationFromAnySimpleType);
        }
        if member.iter().any(|m| m.r#final().union()) {
            return Err(SchemaTypeError::DerivationPreventedByFinalization);
        }
        let variety = Variety::Union(member);
        Ok(Self::derive(
            SCHEMA_BUILTIN_ANY_SIMPLE_TYPE.clone(),
            variety,
        ))
    }

    fn derive(base: Arc<Self>, variety: Variety) -> SimpleTypeDefinitionBuilder {
        let mut facets = match *base {
            SimpleTypeDefinition::AnySimpleType
            | SimpleTypeDefinition::BuiltinPrimitive(_)
            | SimpleTypeDefinition::BuiltinDerived(_) => Facets::default(),
            SimpleTypeDefinition::UserDefined(ref user) => user.facets.derive(),
        };
        if matches!(variety, Variety::List(_)) {
            facets.whitespace = Some(Whitespace::Collapse);
        }
        SimpleTypeDefinitionBuilder {
            base,
            name: None,
            target_namespace: None,
            variety,
            facets,
            r#final: FinalSet::default(),
        }
    }

    pub fn name(&self) -> &str {
        match self {
            Self::AnySimpleType => "anySimpleType",
            Self::BuiltinPrimitive(prim) => prim.name(),
            Self::BuiltinDerived(deri) => deri.name(),
            Self::UserDefined(user) => user.name(),
        }
    }

    pub fn target_namespace(&self) -> Option<&str> {
        match self {
            Self::AnySimpleType => Some(XML_SCHEMA_NAMESPACE),
            Self::BuiltinPrimitive(prim) => Some(prim.target_namespace()),
            Self::BuiltinDerived(deri) => Some(deri.target_namespace()),
            Self::UserDefined(user) => user.target_namespace(),
        }
    }

    /// # Reference
    /// - [4.1.1 The Simple Type Definition Schema Component](https://www.w3.org/TR/xmlschema-2/#dc-defn)
    ///
    /// ```text
    /// {base type definition}
    ///     If the datatype has been derived by restriction then the Simple Type Definition
    ///     component from which it is derived, otherwise the Simple Type Definition for
    ///     anySimpleType (§4.1.6).
    /// ```
    pub fn base_type_definition(&self) -> Option<Arc<SimpleTypeDefinition>> {
        match self {
            Self::AnySimpleType => None,
            Self::BuiltinPrimitive(prim) => Some(prim.base_type_definition()),
            Self::BuiltinDerived(deriv) => Some(deriv.base_type_definition()),
            Self::UserDefined(user) => Some(user.base_type_definition()),
        }
    }

    /// # Reference
    /// - [4.1.1 The Simple Type Definition Schema Component](https://www.w3.org/TR/xmlschema-2/#dc-defn)
    ///
    /// ```text
    /// {variety}
    ///     One of {atomic, list, union}. Depending on the value of {variety},
    ///     further properties are defined as follows:
    ///     atomic
    ///         {primitive type definition}
    ///             A built-in primitive datatype definition.
    ///     list
    ///         {item type definition}
    ///             An atomic or union simple type definition.
    ///     union
    ///         {member type definitions}
    ///             A non-empty sequence of simple type definitions.
    /// ```
    pub fn variety(&self) -> Option<Variety> {
        match self {
            Self::AnySimpleType => None,
            Self::BuiltinPrimitive(prim) => Some(prim.variety()),
            Self::BuiltinDerived(deriv) => Some(deriv.variety()),
            Self::UserDefined(user) => Some(user.variety()),
        }
    }

    /// # Reference
    /// - [4.2.2 ordered](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#rf-ordered)
    /// - [C.1 Fundamental Facets](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#app-fundamental-facets)
    pub fn ordered(&self) -> Option<Ordered> {
        match self {
            Self::AnySimpleType => {
                // Is this correct ????
                None
            }
            Self::BuiltinPrimitive(prim) => Some(prim.ordered()),
            Self::BuiltinDerived(deriv) => Some(deriv.ordered()),
            Self::UserDefined(user) => Some(user.ordered()),
        }
    }

    /// # Reference
    /// - [4.2.3 bounded](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#rf-bounded)
    ///
    /// ```text
    /// When {variety} is atomic, if one of minInclusive or minExclusive and one of maxInclusive
    /// or maxExclusive are among {facets} , then {value} is true; else {value} is false.
    /// When {variety} is list, if length or both of minLength and maxLength are among {facets},
    /// then {value} is true; else {value} is false.
    /// When {variety} is union, if {value} is true for every member of {member type definitions}
    /// and all members of {member type definitions} share a common ancestor, then {value} is true;
    /// else {value} is false.
    /// ```
    pub fn bounded(&self) -> bool {
        match self {
            Self::AnySimpleType => false,
            Self::BuiltinPrimitive(prim) => prim.bounded(),
            Self::BuiltinDerived(deriv) => deriv.bounded(),
            Self::UserDefined(user) => user.bounded(),
        }
    }

    /// # Reference
    /// - [4.2.4 cardinality](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#rf-cardinality)
    ///
    /// ```text
    /// When {variety} is atomic and {value} of {base type definition} is finite, then {value} is finite.
    /// When {variety} is atomic and {value} of {base type definition} is countably infinite and either
    /// of the following conditions are true, then {value} is finite; else {value} is countably infinite:
    ///     1. one of length, maxLength, totalDigits is among {facets},
    ///     2. all of the following are true:
    ///         a. one of minInclusive or minExclusive is among {facets}
    ///         b. one of maxInclusive or maxExclusive is among {facets}
    ///         c. either of the following are true:
    ///             i.  fractionDigits is among {facets}
    ///             ii. {base type definition} is one of date, gYearMonth, gYear, gMonthDay, gDay
    ///                 or gMonth or any type derived from them
    /// When {variety} is list, if length or both of minLength and maxLength are among {facets},
    /// then {value} is finite; else {value} is countably infinite.
    /// When {variety} is union, if {value} is finite for every member of {member type definitions},
    /// then {value} is finite; else {value} is countably infinite.
    /// ```
    pub fn cardinality(&self) -> Cardinality {
        match self {
            Self::AnySimpleType => {
                // Is this correct ????
                Cardinality::CountablyInfinite
            }
            Self::BuiltinPrimitive(prim) => prim.cardinality(),
            Self::BuiltinDerived(deriv) => deriv.cardinality(),
            Self::UserDefined(user) => user.cardinality(),
        }
    }

    /// # Reference
    /// - [4.2.5 numeric](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#rf-numeric)
    ///
    /// ```text
    /// When {variety} is atomic, {value} is inherited from {value} of {base type definition}.
    /// For all primitive types {value} is as specified in the table in Fundamental Facets (§C.1).
    /// When {variety} is list, {value} is false.
    /// When {variety} is union, if {value} is true for every member of {member type definitions},
    /// then {value} is true; else {value} is false.
    /// ```
    pub fn numeric(&self) -> bool {
        match self {
            Self::AnySimpleType => false,
            Self::BuiltinPrimitive(prim) => prim.numeric(),
            Self::BuiltinDerived(deriv) => deriv.numeric(),
            Self::UserDefined(user) => user.numeric(),
        }
    }

    pub fn length(&self) -> Option<i32> {
        match self {
            Self::AnySimpleType | Self::BuiltinPrimitive(_) | Self::BuiltinDerived(_) => None,
            Self::UserDefined(user) => user.length(),
        }
    }

    pub fn min_length(&self) -> Option<i32> {
        match self {
            Self::AnySimpleType | Self::BuiltinPrimitive(_) | Self::BuiltinDerived(_) => None,
            Self::UserDefined(user) => user.min_length(),
        }
    }

    pub fn max_length(&self) -> Option<i32> {
        match self {
            Self::AnySimpleType | Self::BuiltinPrimitive(_) | Self::BuiltinDerived(_) => None,
            Self::UserDefined(user) => user.max_length(),
        }
    }

    pub fn whitespace(&self) -> Option<Whitespace> {
        match self {
            Self::AnySimpleType => None,
            Self::BuiltinPrimitive(ty) => Some(ty.whitespace()),
            Self::BuiltinDerived(ty) => Some(ty.whitespace()),
            Self::UserDefined(user) => user.whitespace(),
        }
    }

    pub fn max_inclusive(&self) -> Option<&SchemaValue> {
        match self {
            Self::AnySimpleType | Self::BuiltinPrimitive(_) | Self::BuiltinDerived(_) => None,
            Self::UserDefined(user) => user.max_inclusive(),
        }
    }

    pub fn max_exclusive(&self) -> Option<&SchemaValue> {
        match self {
            Self::AnySimpleType | Self::BuiltinPrimitive(_) | Self::BuiltinDerived(_) => None,
            Self::UserDefined(user) => user.max_exclusive(),
        }
    }

    pub fn min_inclusive(&self) -> Option<&SchemaValue> {
        match self {
            Self::AnySimpleType | Self::BuiltinPrimitive(_) | Self::BuiltinDerived(_) => None,
            Self::UserDefined(user) => user.min_inclusive(),
        }
    }

    pub fn min_exclusive(&self) -> Option<&SchemaValue> {
        match self {
            Self::AnySimpleType | Self::BuiltinPrimitive(_) | Self::BuiltinDerived(_) => None,
            Self::UserDefined(user) => user.min_exclusive(),
        }
    }

    pub fn total_digits(&self) -> Option<i32> {
        match self {
            Self::AnySimpleType | Self::BuiltinPrimitive(_) | Self::BuiltinDerived(_) => None,
            Self::UserDefined(user) => user.total_digits(),
        }
    }

    pub fn fraction_digits(&self) -> Option<i32> {
        match self {
            Self::AnySimpleType | Self::BuiltinPrimitive(_) | Self::BuiltinDerived(_) => None,
            Self::UserDefined(user) => user.fraction_digits(),
        }
    }

    pub fn r#final(&self) -> FinalSet {
        match self {
            Self::AnySimpleType | Self::BuiltinPrimitive(_) | Self::BuiltinDerived(_) => {
                FinalSet::default()
            }
            Self::UserDefined(user) => user.r#final(),
        }
    }

    /// # Reference
    /// - [Schema Component Constraint: applicable facets](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#defn-coss)
    pub fn is_applicable_facets(&self, facet: FacetType) -> bool {
        match self {
            SimpleTypeDefinition::AnySimpleType => false,
            SimpleTypeDefinition::BuiltinPrimitive(prim) => prim.is_applicable_facets(facet),
            SimpleTypeDefinition::BuiltinDerived(driv) => driv.is_applicable_facets(facet),
            SimpleTypeDefinition::UserDefined(user) => user.is_applicable_facets(facet),
        }
    }

    pub fn parse(
        &self,
        value: &str,
        namespaces: &NamespaceStack,
    ) -> Result<SchemaValue, ParseError> {
        match self {
            Self::AnySimpleType => Err(ParseError::AnySimpleType),
            Self::BuiltinPrimitive(prim) => prim.parse(value, namespaces),
            Self::BuiltinDerived(deriv) => deriv.parse(value, namespaces),
            Self::UserDefined(user) => user.parse(value, namespaces),
        }
    }
}

/// # Note
/// It does not necessarily return the latest common ancestor, but it guarantees that if
/// there is a common ancestor other than `anySimpleType`, it will return that ancestor.
/// In most cases, it will return one of the built-in types.
///
/// List types are treated as having no parent and return `anySimpleType`.
fn common_ancestor(types: &[Arc<SimpleTypeDefinition>]) -> Arc<SimpleTypeDefinition> {
    fn common_ancestor_of_two_types(
        l: Arc<SimpleTypeDefinition>,
        r: Arc<SimpleTypeDefinition>,
    ) -> Arc<SimpleTypeDefinition> {
        match l.variety().zip(r.variety()) {
            None => SCHEMA_BUILTIN_ANY_SIMPLE_TYPE.clone(),
            Some((Variety::List(_), _) | (_, Variety::List(_))) => {
                SCHEMA_BUILTIN_ANY_SIMPLE_TYPE.clone()
            }
            Some((Variety::Atomic(la), Variety::Atomic(ra))) => {
                if la == ra {
                    la.into()
                } else {
                    SCHEMA_BUILTIN_ANY_SIMPLE_TYPE.clone()
                }
            }
            Some((Variety::Atomic(la), Variety::Union(ra)))
            | Some((Variety::Union(ra), Variety::Atomic(la))) => {
                common_ancestor_of_two_types(la.into(), common_ancestor(&ra))
            }
            Some((Variety::Union(la), Variety::Union(ra))) => {
                common_ancestor_of_two_types(common_ancestor(&la), common_ancestor(&ra))
            }
        }
    }

    types
        .iter()
        .cloned()
        .reduce(common_ancestor_of_two_types)
        .unwrap_or_else(|| SCHEMA_BUILTIN_ANY_SIMPLE_TYPE.clone())
}

/// Builtin primitive types.
///
/// # Reference
/// - [3.2 Primitive datatypes](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#built-in-primitive-datatypes)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuiltinPrimitiveType {
    /// [3.2.1 string](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#string)
    String,
    /// [3.2.2 boolean](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#boolean)
    Boolean,
    /// [3.2.3 decimal](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#decimal)
    Decimal,
    /// [3.2.4 float](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#float)
    Float,
    /// [3.2.5 double](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#double)
    Double,
    /// [3.2.6 duration](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#duration)
    Duration,
    /// [3.2.7 dateTime](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#dateTime)
    DateTime,
    /// [3.2.8 time](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#time)
    Time,
    /// [3.2.9 date](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#date)
    Date,
    /// [3.2.10 gYearMonth](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#gYearMonth)
    GYearMonth,
    /// [3.2.11 gYear](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#gYear)
    GYear,
    /// [3.2.12 gMonthDay](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#gMonthDay)
    GMonthDay,
    /// [3.2.13 gDay](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#gDay)
    GDay,
    /// [3.2.14 gMonth](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#gMonth)
    GMonth,
    /// [3.2.15 hexBinary](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#hexBinary)
    HexBinary,
    /// [3.2.16 base64Binary](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#base64Binary)
    Base64Binary,
    /// [3.2.17 anyURI](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#anyURI)
    AnyURI,
    /// [3.2.18 QName](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#QName)
    QName,
    /// [3.2.19 NOTATION](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#NOTATION)
    NOTATION,
}

impl BuiltinPrimitiveType {
    pub fn name(&self) -> &'static str {
        match self {
            Self::String => "string",
            Self::Boolean => "boolean",
            Self::Decimal => "decimal",
            Self::Float => "float",
            Self::Double => "double",
            Self::Duration => "duration",
            Self::DateTime => "dateTime",
            Self::Time => "time",
            Self::Date => "date",
            Self::GYearMonth => "gYearMonth",
            Self::GYear => "gYear",
            Self::GMonthDay => "gMonthDay",
            Self::GDay => "gDay",
            Self::GMonth => "gMonth",
            Self::HexBinary => "hexBinary",
            Self::Base64Binary => "base64Binary",
            Self::AnyURI => "anyURI",
            Self::QName => "QName",
            Self::NOTATION => "NOTATION",
        }
    }

    pub fn target_namespace(&self) -> &'static str {
        XML_SCHEMA_NAMESPACE
    }

    /// # Reference
    /// - [4.1.1 The Simple Type Definition Schema Component](https://www.w3.org/TR/xmlschema-2/#dc-defn)
    ///
    /// ```text
    /// {base type definition}
    ///     If the datatype has been derived by restriction then the Simple Type Definition
    ///     component from which it is derived, otherwise the Simple Type Definition for
    ///     anySimpleType (§4.1.6).
    /// ```
    pub fn base_type_definition(&self) -> Arc<SimpleTypeDefinition> {
        SCHEMA_BUILTIN_ANY_SIMPLE_TYPE.clone()
    }

    /// # Reference
    /// - [4.1.1 The Simple Type Definition Schema Component](https://www.w3.org/TR/xmlschema-2/#dc-defn)
    ///
    /// ```text
    /// {variety}
    ///     One of {atomic, list, union}. Depending on the value of {variety},
    ///     further properties are defined as follows:
    ///     atomic
    ///         {primitive type definition}
    ///             A built-in primitive datatype definition.
    ///     list
    ///         {item type definition}
    ///             An atomic or union simple type definition.
    ///     union
    ///         {member type definitions}
    ///             A non-empty sequence of simple type definitions.
    /// ```
    pub fn variety(&self) -> Variety {
        Variety::Atomic(*self)
    }

    /// # Reference
    /// - [4.2.2 ordered](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#rf-ordered)
    /// - [C.1 Fundamental Facets](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#app-fundamental-facets)
    pub fn ordered(&self) -> Ordered {
        match self {
            Self::String
            | Self::Boolean
            | Self::HexBinary
            | Self::Base64Binary
            | Self::AnyURI
            | Self::QName
            | Self::NOTATION => Ordered::False,
            Self::Decimal => Ordered::Total,
            Self::Float
            | Self::Double
            | Self::Duration
            | Self::DateTime
            | Self::Time
            | Self::Date
            | Self::GYearMonth
            | Self::GYear
            | Self::GMonthDay
            | Self::GDay
            | Self::GMonth => Ordered::Partial,
        }
    }

    /// # Reference
    /// - [4.2.3 bounded](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#rf-bounded)
    /// - [C.1 Fundamental Facets](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#app-fundamental-facets)
    pub fn bounded(&self) -> bool {
        matches!(*self, Self::Float | Self::Double)
    }

    /// # Reference
    /// - [4.2.4 cardinality](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#rf-cardinality)
    /// - [C.1 Fundamental Facets](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#app-fundamental-facets)
    pub fn cardinality(&self) -> Cardinality {
        match self {
            Self::Boolean | Self::Float | Self::Double => Cardinality::Finite,
            _ => Cardinality::CountablyInfinite,
        }
    }

    /// # Reference
    /// - [4.2.5 numeric](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#rf-numeric)
    /// - [C.1 Fundamental Facets](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#app-fundamental-facets)
    pub fn numeric(&self) -> bool {
        matches!(*self, Self::Float | Self::Double | Self::Decimal)
    }

    /// # Reference
    /// - [4.3.6 whiteSpace](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#rf-whiteSpace)
    pub fn whitespace(&self) -> Whitespace {
        match self {
            Self::String => Whitespace::Preserve,
            _ => Whitespace::Collapse,
        }
    }

    pub fn r#final(&self) -> FinalSet {
        FinalSet::default()
    }

    /// # Reference
    /// - [Schema Component Constraint: applicable facets](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#defn-coss)
    pub fn is_applicable_facets(&self, facet: FacetType) -> bool {
        use FacetType::*;

        match self {
            Self::String => {
                matches!(
                    facet,
                    Length | MinLength | MaxLength | Pattern | Enumeration | WhiteSpace
                )
            }
            Self::Boolean => matches!(facet, Pattern | WhiteSpace),
            Self::Decimal => matches!(
                facet,
                TotalDigits
                    | FractionDigits
                    | Pattern
                    | WhiteSpace
                    | Enumeration
                    | MaxInclusive
                    | MaxExclusive
                    | MinInclusive
                    | MinExclusive
            ),
            Self::Float
            | Self::Double
            | Self::Duration
            | Self::DateTime
            | Self::Time
            | Self::GYearMonth
            | Self::GYear
            | Self::GMonthDay
            | Self::GDay
            | Self::GMonth => matches!(
                facet,
                Pattern
                    | Enumeration
                    | WhiteSpace
                    | MaxInclusive
                    | MaxExclusive
                    | MinInclusive
                    | MinExclusive
            ),
            Self::Date => false,
            Self::HexBinary | Self::Base64Binary | Self::AnyURI | Self::QName | Self::NOTATION => {
                matches!(
                    facet,
                    Length | MinLength | MaxLength | Pattern | Enumeration | WhiteSpace
                )
            }
        }
    }

    /// Validate whether `value` is included in the lexical space of this type.
    ///
    /// # Note
    /// This method does not perform any normalization.  \
    /// If necessary, the responsibility for normalization lies with the caller of the method.
    ///
    /// # Reference
    /// - [2.3 Lexical space](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#lexical-space)
    pub fn validate(&self, value: &str) -> bool {
        match self {
            Self::String => value.chars().all(|c| XMLVersion::default().is_char(c)),
            Self::Boolean => matches!(value, "true" | "false" | "1" | "0"),
            Self::Decimal => {
                // 3.2.3.1 Lexical representation
                let rem = value
                    .strip_prefix(|c| matches!(c, '+' | '-'))
                    .unwrap_or(value)
                    .trim_start_matches(|c: char| c.is_ascii_digit());
                if rem.is_empty() {
                    return true;
                }
                rem.strip_prefix('.').is_some_and(|rem| {
                    rem.trim_start_matches(|c: char| c.is_ascii_digit())
                        .is_empty()
                })
            }
            Self::Float => {
                // 3.2.4.1 Lexical representation
                if value.eq_ignore_ascii_case("INF")
                    || value.eq_ignore_ascii_case("-INF")
                    || value.eq_ignore_ascii_case("NaN")
                {
                    // The `FromStr` for `f32` does not consider case, but according to the
                    // XML Schema specification, only uppercase characters are valid values.
                    return value.chars().all(|c| c.is_ascii_uppercase());
                }
                value.parse::<f32>().is_ok()
            }
            Self::Double => {
                // 3.2.5.1 Lexical representation
                if value.eq_ignore_ascii_case("INF")
                    || value.eq_ignore_ascii_case("-INF")
                    || value.eq_ignore_ascii_case("NaN")
                {
                    // The `FromStr` for `f32` does not consider case, but according to the
                    // XML Schema specification, only uppercase characters are valid values.
                    return value.chars().all(|c| c.is_ascii_uppercase());
                }
                value.parse::<f64>().is_ok()
            }
            Self::Duration => value.parse::<Duration>().is_ok(),
            Self::DateTime => value.parse::<DateTime>().is_ok(),
            Self::Time => value.parse::<Time>().is_ok(),
            Self::Date => value.parse::<Date>().is_ok(),
            Self::GYearMonth => value.parse::<GYearMonth>().is_ok(),
            Self::GYear => value.parse::<GYear>().is_ok(),
            Self::GMonthDay => value.parse::<GMonthDay>().is_ok(),
            Self::GDay => value.parse::<GDay>().is_ok(),
            Self::GMonth => value.parse::<GMonth>().is_ok(),
            Self::HexBinary => value.chars().all(|c| c.is_ascii_hexdigit()),
            Self::Base64Binary => Base64Binary::from_encoded(value.bytes(), false).is_ok(),
            Self::AnyURI => URIString::parse(value).is_ok(),
            Self::QName => XMLVersion::default().validate_qname(value),
            Self::NOTATION => {
                // The following SCCs cannot appear directly in the schema.
                // Therefore, directly validating values against this type will result
                // in an error.
                //
                // 3.2.19 NOTATION
                //   [SCC: enumeration facet value required for NOTATION]
                //     It is an ·error· for NOTATION to be used directly in a schema.
                //     Only datatypes that are ·derived· from NOTATION by specifying
                //     a value for ·enumeration· can be used in a schema.
                false
            }
        }
    }

    pub fn parse(
        &self,
        value: &str,
        namespaces: &NamespaceStack,
    ) -> Result<SchemaValue, ParseError> {
        let collapsed = value.trim_matches(|c| XMLVersion::default().is_whitespace(c));
        match self {
            Self::String => Ok(SchemaValue::String(value.into())),
            Self::Boolean => match collapsed {
                "true" | "1" => Ok(SchemaValue::Boolean(true)),
                "false" | "0" => Ok(SchemaValue::Boolean(false)),
                _ => Err(ParseError::Boolean),
            },
            Self::Decimal => {
                // 3.2.3.1 Lexical representation
                let (l, r) = collapsed.split_once('.').unwrap_or((collapsed, "0"));
                if l.strip_prefix(['+', '-'])
                    .unwrap_or(l)
                    .bytes()
                    .any(|b| !b.is_ascii_digit())
                    || r.bytes().any(|b| !b.is_ascii_digit())
                {
                    return Err(ParseError::Decimal);
                }
                let l = if let Some(l) = l.strip_prefix('+') {
                    let l = l.trim_start_matches('0');
                    if l.is_empty() { "0".into() } else { l.into() }
                } else if l.starts_with('-') {
                    l.into()
                } else {
                    let l = l.trim_start_matches('0');
                    if l.is_empty() { "0".into() } else { l.into() }
                };
                let r = r.trim_end_matches('0');
                Ok(SchemaValue::Decimal(
                    l,
                    if r.is_empty() { "0".into() } else { r.into() },
                ))
            }
            Self::Float => {
                if collapsed.eq_ignore_ascii_case("INF")
                    || collapsed.eq_ignore_ascii_case("-INF")
                    || collapsed.eq_ignore_ascii_case("NaN")
                {
                    return match collapsed {
                        "INF" => Ok(SchemaValue::Float(f32::INFINITY)),
                        "-INF" => Ok(SchemaValue::Float(f32::NEG_INFINITY)),
                        "NaN" => Ok(SchemaValue::Float(f32::NAN)),
                        _ => Err(ParseError::Float),
                    };
                }
                let f = collapsed.parse::<f32>().map_err(|_| ParseError::Float)?;
                if f.is_infinite() || f.is_nan() {
                    // If any expression other than "INF", "-INF", or "NaN" evaluates
                    // to one of these values, an error occurs.
                    Err(ParseError::Float)
                } else {
                    Ok(SchemaValue::Float(f))
                }
            }
            Self::Double => {
                if collapsed.eq_ignore_ascii_case("INF")
                    || collapsed.eq_ignore_ascii_case("-INF")
                    || collapsed.eq_ignore_ascii_case("NaN")
                {
                    return match collapsed {
                        "INF" => Ok(SchemaValue::Double(f64::INFINITY)),
                        "-INF" => Ok(SchemaValue::Double(f64::NEG_INFINITY)),
                        "NaN" => Ok(SchemaValue::Double(f64::NAN)),
                        _ => Err(ParseError::Double),
                    };
                }
                let f = collapsed.parse::<f64>().map_err(|_| ParseError::Double)?;
                if f.is_infinite() || f.is_nan() {
                    // If any expression other than "INF", "-INF", or "NaN" evaluates
                    // to one of these values, an error occurs.
                    Err(ParseError::Double)
                } else {
                    Ok(SchemaValue::Double(f))
                }
            }
            Self::Duration => Ok(SchemaValue::Duration(
                collapsed
                    .parse::<Duration>()
                    .map_err(|_| ParseError::Duration)?,
            )),
            Self::DateTime => Ok(SchemaValue::DateTime(
                collapsed
                    .parse::<DateTime>()
                    .map_err(|_| ParseError::Duration)?,
            )),
            Self::Time => Ok(SchemaValue::Time(
                collapsed.parse::<Time>().map_err(|_| ParseError::Time)?,
            )),
            Self::Date => Ok(SchemaValue::Date(
                collapsed.parse::<Date>().map_err(|_| ParseError::Date)?,
            )),
            Self::GYearMonth => Ok(SchemaValue::GYearMonth(
                collapsed
                    .parse::<GYearMonth>()
                    .map_err(|_| ParseError::GYearMonth)?,
            )),
            Self::GYear => Ok(SchemaValue::GYear(
                collapsed.parse::<GYear>().map_err(|_| ParseError::GYear)?,
            )),
            Self::GMonthDay => Ok(SchemaValue::GMonthDay(
                collapsed
                    .parse::<GMonthDay>()
                    .map_err(|_| ParseError::GMonthDay)?,
            )),
            Self::GDay => Ok(SchemaValue::GDay(
                collapsed.parse::<GDay>().map_err(|_| ParseError::GDay)?,
            )),
            Self::GMonth => Ok(SchemaValue::GMonth(
                collapsed
                    .parse::<GMonth>()
                    .map_err(|_| ParseError::GMonth)?,
            )),
            Self::HexBinary => {
                if collapsed.len() % 2 != 0 {
                    return Err(ParseError::HexBinary);
                }
                if collapsed.bytes().any(|b| !b.is_ascii_hexdigit()) {
                    return Err(ParseError::HexBinary);
                }
                Ok(SchemaValue::HexBinary(
                    collapsed.to_ascii_uppercase().into(),
                ))
            }
            Self::Base64Binary => Ok(SchemaValue::Base64Binary(
                Base64Binary::from_encoded(collapsed.bytes(), true)
                    .map_err(|_| ParseError::Base64Binary)?,
            )),
            Self::AnyURI => {
                let uri = URIString::parse(collapsed).map_err(|_| ParseError::URI)?;
                Ok(SchemaValue::AnyURI(uri.into()))
            }
            Self::QName => {
                if !XMLVersion::default().validate_qname(collapsed) {
                    return Err(ParseError::QName);
                }
                if let Some((prefix, local)) = collapsed.split_once(':') {
                    if let Some(namespace) = namespaces.get(prefix) {
                        Ok(SchemaValue::QName(
                            Some(namespace.namespace_name),
                            local.into(),
                        ))
                    } else {
                        Err(ParseError::QName)
                    }
                } else if let Some(namespace) = namespaces.get("") {
                    Ok(SchemaValue::QName(
                        Some(namespace.namespace_name),
                        collapsed.into(),
                    ))
                } else {
                    Ok(SchemaValue::QName(None, collapsed.into()))
                }
            }
            Self::NOTATION => {
                if !XMLVersion::default().validate_qname(collapsed) {
                    return Err(ParseError::NOTATION);
                }
                if let Some((prefix, local)) = collapsed.split_once(':') {
                    if let Some(namespace) = namespaces.get(prefix) {
                        Ok(SchemaValue::NOTATION(
                            Some(namespace.namespace_name),
                            local.into(),
                        ))
                    } else {
                        Err(ParseError::NOTATION)
                    }
                } else if let Some(namespace) = namespaces.get("") {
                    Ok(SchemaValue::NOTATION(
                        Some(namespace.namespace_name),
                        collapsed.into(),
                    ))
                } else {
                    Ok(SchemaValue::NOTATION(None, collapsed.into()))
                }
            }
        }
    }
}

impl From<BuiltinPrimitiveType> for Arc<SimpleTypeDefinition> {
    fn from(value: BuiltinPrimitiveType) -> Self {
        match value {
            BuiltinPrimitiveType::String => SCHEMA_BUILTIN_STRING.clone(),
            BuiltinPrimitiveType::Boolean => SCHEMA_BUILTIN_BOOLEAN.clone(),
            BuiltinPrimitiveType::Decimal => SCHEMA_BUILTIN_DECIMAL.clone(),
            BuiltinPrimitiveType::Float => SCHEMA_BUILTIN_FLOAT.clone(),
            BuiltinPrimitiveType::Double => SCHEMA_BUILTIN_DOUBLE.clone(),
            BuiltinPrimitiveType::Duration => SCHEMA_BUILTIN_DURATION.clone(),
            BuiltinPrimitiveType::DateTime => SCHEMA_BUILTIN_DATETIME.clone(),
            BuiltinPrimitiveType::Time => SCHEMA_BUILTIN_TIME.clone(),
            BuiltinPrimitiveType::Date => SCHEMA_BUILTIN_DATE.clone(),
            BuiltinPrimitiveType::GYearMonth => SCHEMA_BUILTIN_GYEARMONTH.clone(),
            BuiltinPrimitiveType::GYear => SCHEMA_BUILTIN_GYEAR.clone(),
            BuiltinPrimitiveType::GMonthDay => SCHEMA_BUILTIN_GMONTHDAY.clone(),
            BuiltinPrimitiveType::GDay => SCHEMA_BUILTIN_GDAY.clone(),
            BuiltinPrimitiveType::GMonth => SCHEMA_BUILTIN_GMONTH.clone(),
            BuiltinPrimitiveType::HexBinary => SCHEMA_BUILTIN_HEXBINARY.clone(),
            BuiltinPrimitiveType::Base64Binary => SCHEMA_BUILTIN_BASE64BINARY.clone(),
            BuiltinPrimitiveType::AnyURI => SCHEMA_BUILTIN_ANYURI.clone(),
            BuiltinPrimitiveType::QName => SCHEMA_BUILTIN_QNAME.clone(),
            BuiltinPrimitiveType::NOTATION => SCHEMA_BUILTIN_NOTATION.clone(),
        }
    }
}

/// Builtin derived types.
///
/// # Reference
/// - [3.3 Derived datatypes](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#built-in-derived)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuiltinDerivedType {
    /// [3.3.1 normalizedString](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#normalizedString)
    NormalizedString,
    /// [3.3.2 token](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#token)
    Token,
    /// [3.3.3 language](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#language)
    Language,
    /// [3.3.4 NMTOKEN](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#NMTOKEN)
    NMTOKEN,
    /// [3.3.5 NMTOKENS](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#NMTOKENS)
    NMTOKENS,
    /// [3.3.6 Name](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#Name)
    Name,
    /// [3.3.7 NCName](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#NCName)
    NCName,
    /// [3.3.8 ID](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#ID)
    ID,
    /// [3.3.9 IDREF](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#IDREF)
    IDREF,
    /// [3.3.10 IDREFS](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#IDREFS)
    IDREFS,
    /// [3.3.11 ENTITY](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#ENTITY)
    ENTITY,
    /// [3.3.12 ENTITIES](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#ENTITIES)
    ENTITIES,
    /// [3.3.13 integer](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#integer)
    Integer,
    /// [3.3.14 nonPositiveInteger](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#nonPositiveInteger)
    NonPositiveInteger,
    /// [3.3.15 negativeInteger](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#negativeInteger)
    NegativeInteger,
    /// [3.3.16 long](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#long)
    Long,
    /// [3.3.17 int](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#int)
    Int,
    /// [3.3.18 short](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#short)
    Short,
    /// [3.3.19 byte](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#byte)
    Byte,
    /// [3.3.20 nonNegativeInteger](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#nonNegativeInteger)
    NonNegativeInteger,
    /// [3.3.21 unsignedLong](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#unsignedLong)
    UnsignedLong,
    /// [3.3.22 unsignedInt](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#unsignedInt)
    UnsignedInt,
    /// [3.3.23 unsignedShort](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#unsignedShort)
    UnsignedShort,
    /// [3.3.24 unsignedByte](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#unsignedByte)
    UnsignedByte,
    /// [3.3.25 positiveInteger](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#positiveInteger)
    PositiveInteger,
}

impl BuiltinDerivedType {
    pub fn name(&self) -> &'static str {
        match self {
            Self::NormalizedString => "normalizedString",
            Self::Token => "token",
            Self::Language => "language",
            Self::NMTOKEN => "NMTOKEN",
            Self::NMTOKENS => "NMTOKENS",
            Self::Name => "Name",
            Self::NCName => "NCName",
            Self::ID => "ID",
            Self::IDREF => "IDREF",
            Self::IDREFS => "IDREFS",
            Self::ENTITY => "ENTITY",
            Self::ENTITIES => "ENTITIES",
            Self::Integer => "integer",
            Self::NonPositiveInteger => "nonPositiveInteger",
            Self::NegativeInteger => "negativeInteger",
            Self::Long => "long",
            Self::Int => "int",
            Self::Short => "short",
            Self::Byte => "byte",
            Self::NonNegativeInteger => "nonNegativeInteger",
            Self::UnsignedLong => "unsignedLong",
            Self::UnsignedInt => "unsignedInt",
            Self::UnsignedShort => "unsignedShort",
            Self::UnsignedByte => "unsignedByte",
            Self::PositiveInteger => "positiveInteger",
        }
    }

    pub fn target_namespace(&self) -> &'static str {
        XML_SCHEMA_NAMESPACE
    }

    /// # Reference
    /// - [4.1.1 The Simple Type Definition Schema Component](https://www.w3.org/TR/xmlschema-2/#dc-defn)
    ///
    /// ```text
    /// {base type definition}
    ///     If the datatype has been derived by restriction then the Simple Type Definition
    ///     component from which it is derived, otherwise the Simple Type Definition for
    ///     anySimpleType (§4.1.6).
    /// ```
    pub fn base_type_definition(&self) -> Arc<SimpleTypeDefinition> {
        match self {
            Self::NormalizedString => SCHEMA_BUILTIN_STRING.clone(),
            Self::Token => SCHEMA_BUILTIN_NORMALIZED_STRING.clone(),
            Self::Language | Self::NMTOKEN | Self::Name => SCHEMA_BUILTIN_TOKEN.clone(),
            Self::NMTOKENS => {
                // Since this is derived by list of NMTOKEN (not derived by restriction),
                // it returns anySimpleType.
                SCHEMA_BUILTIN_ANY_SIMPLE_TYPE.clone()
            }
            Self::NCName => SCHEMA_BUILTIN_NAME.clone(),
            Self::ID | Self::IDREF | Self::ENTITY => SCHEMA_BUILTIN_NCNAME.clone(),
            Self::IDREFS => {
                // Since this is derived by list of IDREF (not derived by restriction),
                // it returns anySimpleType.
                SCHEMA_BUILTIN_ANY_SIMPLE_TYPE.clone()
            }
            Self::ENTITIES => {
                // Since this is derived by list of ENTITY (not derived by restriction),
                // it returns anySimpleType.
                SCHEMA_BUILTIN_ANY_SIMPLE_TYPE.clone()
            }
            Self::Integer => SCHEMA_BUILTIN_DECIMAL.clone(),
            Self::NonPositiveInteger | Self::Long | Self::NonNegativeInteger => {
                SCHEMA_BUILTIN_INTEGER.clone()
            }
            Self::NegativeInteger => SCHEMA_BUILTIN_NONPOSITIVE_INTEGER.clone(),
            Self::Int => SCHEMA_BUILTIN_LONG.clone(),
            Self::Short => SCHEMA_BUILTIN_INT.clone(),
            Self::Byte => SCHEMA_BUILTIN_SHORT.clone(),
            Self::UnsignedLong | Self::PositiveInteger => {
                SCHEMA_BUILTIN_NONNEGATIVE_INTEGER.clone()
            }
            Self::UnsignedInt => SCHEMA_BUILTIN_UNSIGNED_LONG.clone(),
            Self::UnsignedShort => SCHEMA_BUILTIN_UNSIGNED_INT.clone(),
            Self::UnsignedByte => SCHEMA_BUILTIN_UNSIGNED_SHORT.clone(),
        }
    }

    /// # Reference
    /// - [4.1.1 The Simple Type Definition Schema Component](https://www.w3.org/TR/xmlschema-2/#dc-defn)
    ///
    /// ```text
    /// {variety}
    ///     One of {atomic, list, union}. Depending on the value of {variety},
    ///     further properties are defined as follows:
    ///     atomic
    ///         {primitive type definition}
    ///             A built-in primitive datatype definition.
    ///     list
    ///         {item type definition}
    ///             An atomic or union simple type definition.
    ///     union
    ///         {member type definitions}
    ///             A non-empty sequence of simple type definitions.
    /// ```
    pub fn variety(&self) -> Variety {
        match self {
            Self::NormalizedString
            | Self::Token
            | Self::Language
            | Self::NMTOKEN
            | Self::Name
            | Self::NCName
            | Self::ID
            | Self::IDREF
            | Self::ENTITY => Variety::Atomic(BuiltinPrimitiveType::String),
            Self::NMTOKENS => Variety::List(SCHEMA_BUILTIN_TOKEN.clone()),
            Self::IDREFS => Variety::List(SCHEMA_BUILTIN_IDREF.clone()),
            Self::ENTITIES => Variety::List(SCHEMA_BUILTIN_ENTITY.clone()),
            Self::Integer
            | Self::NonPositiveInteger
            | Self::Long
            | Self::Int
            | Self::Short
            | Self::Byte
            | Self::NonNegativeInteger
            | Self::NegativeInteger
            | Self::UnsignedLong
            | Self::UnsignedInt
            | Self::UnsignedShort
            | Self::UnsignedByte
            | Self::PositiveInteger => Variety::Atomic(BuiltinPrimitiveType::Decimal),
        }
    }

    /// # Reference
    /// - [4.2.2 ordered](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#rf-ordered)
    /// - [C.1 Fundamental Facets](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#app-fundamental-facets)
    pub fn ordered(&self) -> Ordered {
        match self {
            Self::NormalizedString
            | Self::Token
            | Self::Language
            | Self::NMTOKEN
            | Self::NMTOKENS
            | Self::Name
            | Self::NCName
            | Self::ID
            | Self::IDREF
            | Self::IDREFS
            | Self::ENTITY
            | Self::ENTITIES => Ordered::False,
            Self::Integer
            | Self::NonPositiveInteger
            | Self::NegativeInteger
            | Self::Long
            | Self::Int
            | Self::Short
            | Self::Byte
            | Self::NonNegativeInteger
            | Self::UnsignedLong
            | Self::UnsignedInt
            | Self::UnsignedShort
            | Self::UnsignedByte
            | Self::PositiveInteger => Ordered::Total,
        }
    }

    /// # Reference
    /// - [4.2.3 bounded](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#rf-bounded)
    /// - [C.1 Fundamental Facets](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#app-fundamental-facets)
    pub fn bounded(&self) -> bool {
        matches!(
            *self,
            Self::Long
                | Self::Int
                | Self::Short
                | Self::Byte
                | Self::UnsignedLong
                | Self::UnsignedInt
                | Self::UnsignedShort
                | Self::UnsignedByte
        )
    }

    /// # Reference
    /// - [4.2.4 cardinality](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#rf-cardinality)
    /// - [C.1 Fundamental Facets](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#app-fundamental-facets)
    pub fn cardinality(&self) -> Cardinality {
        match self {
            Self::Long
            | Self::Int
            | Self::Short
            | Self::Byte
            | Self::UnsignedLong
            | Self::UnsignedInt
            | Self::UnsignedShort
            | Self::UnsignedByte => Cardinality::Finite,
            _ => Cardinality::CountablyInfinite,
        }
    }

    /// # Reference
    /// - [4.2.5 numeric](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#rf-numeric)
    /// - [C.1 Fundamental Facets](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#app-fundamental-facets)
    pub fn numeric(&self) -> bool {
        matches!(
            *self,
            Self::Integer
                | Self::NonPositiveInteger
                | Self::NegativeInteger
                | Self::Long
                | Self::Int
                | Self::Short
                | Self::Byte
                | Self::NonNegativeInteger
                | Self::UnsignedLong
                | Self::UnsignedInt
                | Self::UnsignedShort
                | Self::UnsignedByte
                | Self::PositiveInteger
        )
    }

    /// # Reference
    /// - [4.3.6 whiteSpace](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#rf-whiteSpace)
    pub fn whitespace(&self) -> Whitespace {
        match self {
            Self::NormalizedString
            | Self::Token
            | Self::Language
            | Self::Name
            | Self::NMTOKEN
            | Self::NCName
            | Self::ID
            | Self::IDREF
            | Self::ENTITY => Whitespace::Preserve,
            _ => Whitespace::Collapse,
        }
    }

    pub fn r#final(&self) -> FinalSet {
        FinalSet::default()
    }

    /// # Reference
    /// - [Schema Component Constraint: applicable facets](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#defn-coss)
    pub fn is_applicable_facets(&self, facet: FacetType) -> bool {
        use FacetType::*;

        match self {
            Self::NormalizedString
            | Self::Token
            | Self::Language
            | Self::NMTOKEN
            | Self::NMTOKENS
            | Self::Name
            | Self::NCName
            | Self::ID
            | Self::IDREF
            | Self::IDREFS
            | Self::ENTITY
            | Self::ENTITIES => matches!(
                facet,
                Length | MinLength | MaxLength | Pattern | Enumeration | WhiteSpace
            ),
            Self::Integer
            | Self::NonPositiveInteger
            | Self::NegativeInteger
            | Self::Long
            | Self::Int
            | Self::Short
            | Self::Byte
            | Self::NonNegativeInteger
            | Self::UnsignedLong
            | Self::UnsignedInt
            | Self::UnsignedShort
            | Self::UnsignedByte
            | Self::PositiveInteger => {
                matches!(
                    facet,
                    TotalDigits
                        | FractionDigits
                        | Pattern
                        | WhiteSpace
                        | Enumeration
                        | MaxInclusive
                        | MaxExclusive
                        | MinInclusive
                        | MinExclusive
                )
            }
        }
    }

    /// Validate whether `value` is included in the lexical space of this type.
    ///
    /// # Note
    /// This method does not perform any normalization.  \
    /// If necessary, the responsibility for normalization lies with the caller of the method.
    ///
    /// # Reference
    /// - [2.3 Lexical space](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#lexical-space)
    pub fn validate(&self, value: &str) -> bool {
        match self {
            Self::NormalizedString => !value.contains(['\x0D', '\x0A', '\x09']),
            Self::Token => {
                !value.starts_with('\x20')
                    && !value.ends_with('\x20')
                    && !value.contains(['\x0D', '\x0A', '\x09'])
                    && !value.contains("\x20\x20")
            }
            Self::Language => {
                let mut split = value.split('-');
                split.next().is_some_and(|s| {
                    (1..=8).contains(&s.len()) && s.bytes().all(|b| b.is_ascii_alphabetic())
                }) && split.all(|s| {
                    (1..=8).contains(&s.len()) && s.bytes().all(|b| b.is_ascii_alphanumeric())
                })
            }
            Self::NMTOKEN => XMLVersion::default().validate_nmtoken(value),
            Self::NMTOKENS => XMLVersion::default().validate_nmtokens(value),
            Self::Name => XMLVersion::default().validate_name(value),
            Self::NCName | Self::ID | Self::IDREF | Self::ENTITY => {
                XMLVersion::default().validate_ncname(value)
            }
            Self::IDREFS | Self::ENTITIES => value
                .split_ascii_whitespace()
                .all(|s| XMLVersion::default().validate_ncname(s)),
            Self::Integer => {
                let s = value.strip_prefix(['+', '-']).unwrap_or(value);
                s.bytes().all(|b| b.is_ascii_digit())
            }
            Self::NonPositiveInteger => {
                if let Some(s) = value.strip_prefix('+') {
                    s.bytes().all(|b| b == b'0')
                } else if let Some(s) = value.strip_prefix('-') {
                    s.bytes().all(|b| b.is_ascii_digit())
                } else {
                    value.bytes().all(|b| b == b'0')
                }
            }
            Self::NegativeInteger => value
                .strip_prefix('-')
                .is_some_and(|s| s.bytes().all(|b| b.is_ascii_digit())),
            Self::Long => value.parse::<i64>().is_ok(),
            Self::Int => value.parse::<i32>().is_ok(),
            Self::Short => value.parse::<i16>().is_ok(),
            Self::Byte => value.parse::<i8>().is_ok(),
            Self::NonNegativeInteger => {
                if let Some(s) = value.strip_prefix('-') {
                    s.bytes().all(|b| b == b'0')
                } else if let Some(s) = value.strip_prefix('+') {
                    s.bytes().all(|b| b.is_ascii_digit())
                } else {
                    value.bytes().all(|b| b == b'0')
                }
            }
            Self::UnsignedLong => value.parse::<u64>().is_ok(),
            Self::UnsignedInt => value.parse::<u32>().is_ok(),
            Self::UnsignedShort => value.parse::<u16>().is_ok(),
            Self::UnsignedByte => value.parse::<u8>().is_ok(),
            Self::PositiveInteger => value
                .strip_prefix('+')
                .unwrap_or(value)
                .bytes()
                .all(|b| b.is_ascii_digit()),
        }
    }

    pub fn parse(
        &self,
        value: &str,
        _namespaces: &NamespaceStack,
    ) -> Result<SchemaValue, ParseError> {
        let collapsed = value.trim_matches(|c| XMLVersion::default().is_whitespace(c));
        match self {
            Self::NormalizedString => {
                if !value.contains(['\x0D', '\x0A', '\x09']) {
                    Ok(SchemaValue::String(value.into()))
                } else {
                    Err(ParseError::NormalizedString)
                }
            }
            Self::Token => {
                if !value.starts_with('\x20')
                    && !value.ends_with('\x20')
                    && !value.contains(['\x0D', '\x0A', '\x09'])
                    && !value.contains("\x20\x20")
                {
                    Ok(SchemaValue::String(value.into()))
                } else {
                    Err(ParseError::Token)
                }
            }
            Self::Language => {
                let mut split = value.split('-');
                if split.next().is_some_and(|s| {
                    (1..=8).contains(&s.len()) && s.bytes().all(|b| b.is_ascii_alphabetic())
                }) && split.all(|s| {
                    (1..=8).contains(&s.len()) && s.bytes().all(|b| b.is_ascii_alphanumeric())
                }) {
                    Ok(SchemaValue::String(value.into()))
                } else {
                    Err(ParseError::Language)
                }
            }
            Self::NMTOKEN => {
                if XMLVersion::default().validate_nmtoken(value) {
                    Ok(SchemaValue::String(value.into()))
                } else {
                    Err(ParseError::NMTOKEN)
                }
            }
            Self::NMTOKENS => Ok(SchemaValue::List(
                value
                    .split('\x20')
                    .map(|s| Self::NMTOKEN.parse(s, _namespaces))
                    .collect::<Result<Vec<_>, ParseError>>()?
                    .into(),
            )),
            Self::Name => {
                if XMLVersion::default().validate_name(value) {
                    Ok(SchemaValue::String(value.into()))
                } else {
                    Err(ParseError::Name)
                }
            }
            Self::NCName => {
                if XMLVersion::default().validate_ncname(value) {
                    Ok(SchemaValue::String(value.into()))
                } else {
                    Err(ParseError::NCName)
                }
            }
            Self::ID => {
                if XMLVersion::default().validate_ncname(value) {
                    Ok(SchemaValue::String(value.into()))
                } else {
                    Err(ParseError::ID)
                }
            }
            Self::IDREF => {
                if XMLVersion::default().validate_ncname(value) {
                    Ok(SchemaValue::String(value.into()))
                } else {
                    Err(ParseError::IDREF)
                }
            }
            Self::IDREFS => Ok(SchemaValue::List(
                value
                    .split('\x20')
                    .map(|s| Self::IDREF.parse(s, _namespaces))
                    .collect::<Result<Vec<_>, ParseError>>()?
                    .into(),
            )),
            Self::ENTITY => {
                if XMLVersion::default().validate_ncname(value) {
                    Ok(SchemaValue::String(value.into()))
                } else {
                    Err(ParseError::ENTITY)
                }
            }
            Self::ENTITIES => Ok(SchemaValue::List(
                value
                    .split('\x20')
                    .map(|s| Self::ENTITY.parse(s, _namespaces))
                    .collect::<Result<Vec<_>, ParseError>>()?
                    .into(),
            )),
            Self::Integer => {
                let int = collapsed.parse::<i128>().map_err(|_| ParseError::Integer)?;
                Ok(SchemaValue::Integer(int))
            }
            Self::NonPositiveInteger => {
                let check = if let Some(s) = collapsed.strip_prefix('+') {
                    s.bytes().all(|b| b == b'0')
                } else if let Some(s) = collapsed.strip_prefix('-') {
                    s.bytes().all(|b| b.is_ascii_digit())
                } else {
                    collapsed.bytes().all(|b| b == b'0')
                };
                if !check {
                    return Err(ParseError::NonPositiveInteger);
                }
                Ok(SchemaValue::Integer(
                    collapsed
                        .parse::<i128>()
                        .map_err(|_| ParseError::NonPositiveInteger)?,
                ))
            }
            Self::NegativeInteger => {
                if collapsed
                    .strip_prefix('-')
                    .is_some_and(|s| s.bytes().all(|b| b.is_ascii_digit()))
                {
                    Ok(SchemaValue::Integer(
                        collapsed
                            .parse::<i128>()
                            .map_err(|_| ParseError::NonPositiveInteger)?,
                    ))
                } else {
                    Err(ParseError::NegativeInteger)
                }
            }
            Self::Long => Ok(SchemaValue::Integer(
                collapsed.parse::<i64>().map_err(|_| ParseError::Long)? as i128,
            )),
            Self::Int => Ok(SchemaValue::Integer(
                collapsed.parse::<i32>().map_err(|_| ParseError::Int)? as i128,
            )),
            Self::Short => Ok(SchemaValue::Integer(
                collapsed.parse::<i16>().map_err(|_| ParseError::Short)? as i128,
            )),
            Self::Byte => Ok(SchemaValue::Integer(
                collapsed.parse::<i8>().map_err(|_| ParseError::Byte)? as i128,
            )),
            Self::NonNegativeInteger => {
                let check = if let Some(s) = collapsed.strip_prefix('-') {
                    s.bytes().all(|b| b == b'0')
                } else if let Some(s) = collapsed.strip_prefix('+') {
                    s.bytes().all(|b| b.is_ascii_digit())
                } else {
                    collapsed.bytes().all(|b| b.is_ascii_digit())
                };
                if !check {
                    return Err(ParseError::NonNegativeInteger);
                }
                Ok(SchemaValue::Integer(
                    collapsed
                        .parse::<i128>()
                        .map_err(|_| ParseError::NonNegativeInteger)?,
                ))
            }
            Self::UnsignedLong => Ok(SchemaValue::Integer(
                collapsed
                    .parse::<u64>()
                    .map_err(|_| ParseError::UnsignedLong)? as i128,
            )),
            Self::UnsignedInt => Ok(SchemaValue::Integer(
                collapsed
                    .parse::<u32>()
                    .map_err(|_| ParseError::UnsignedInt)? as i128,
            )),
            Self::UnsignedShort => Ok(SchemaValue::Integer(
                collapsed
                    .parse::<u16>()
                    .map_err(|_| ParseError::UnsignedShort)? as i128,
            )),
            Self::UnsignedByte => Ok(SchemaValue::Integer(
                collapsed
                    .parse::<u8>()
                    .map_err(|_| ParseError::UnsignedByte)? as i128,
            )),
            Self::PositiveInteger => {
                if collapsed
                    .strip_prefix('+')
                    .unwrap_or(collapsed)
                    .bytes()
                    .all(|b| b.is_ascii_digit())
                {
                    Ok(SchemaValue::Integer(
                        collapsed
                            .parse::<i128>()
                            .map_err(|_| ParseError::PositiveInteger)?,
                    ))
                } else {
                    Err(ParseError::PositiveInteger)
                }
            }
        }
    }
}

impl From<BuiltinDerivedType> for Arc<SimpleTypeDefinition> {
    fn from(value: BuiltinDerivedType) -> Self {
        match value {
            BuiltinDerivedType::Byte => SCHEMA_BUILTIN_BYTE.clone(),
            BuiltinDerivedType::ENTITIES => SCHEMA_BUILTIN_ENTITIES.clone(),
            BuiltinDerivedType::ENTITY => SCHEMA_BUILTIN_ENTITY.clone(),
            BuiltinDerivedType::ID => SCHEMA_BUILTIN_ID.clone(),
            BuiltinDerivedType::IDREF => SCHEMA_BUILTIN_IDREF.clone(),
            BuiltinDerivedType::IDREFS => SCHEMA_BUILTIN_IDREFS.clone(),
            BuiltinDerivedType::Int => SCHEMA_BUILTIN_INT.clone(),
            BuiltinDerivedType::Integer => SCHEMA_BUILTIN_INTEGER.clone(),
            BuiltinDerivedType::Language => SCHEMA_BUILTIN_LANGUAGE.clone(),
            BuiltinDerivedType::Long => SCHEMA_BUILTIN_LONG.clone(),
            BuiltinDerivedType::NCName => SCHEMA_BUILTIN_NCNAME.clone(),
            BuiltinDerivedType::NMTOKEN => SCHEMA_BUILTIN_NMTOKEN.clone(),
            BuiltinDerivedType::NMTOKENS => SCHEMA_BUILTIN_NMTOKENS.clone(),
            BuiltinDerivedType::Name => SCHEMA_BUILTIN_NAME.clone(),
            BuiltinDerivedType::NegativeInteger => SCHEMA_BUILTIN_NEGATIVE_INTEGER.clone(),
            BuiltinDerivedType::NonNegativeInteger => SCHEMA_BUILTIN_NONNEGATIVE_INTEGER.clone(),
            BuiltinDerivedType::NonPositiveInteger => SCHEMA_BUILTIN_NONPOSITIVE_INTEGER.clone(),
            BuiltinDerivedType::NormalizedString => SCHEMA_BUILTIN_NORMALIZED_STRING.clone(),
            BuiltinDerivedType::PositiveInteger => SCHEMA_BUILTIN_POSITIVE_INTEGER.clone(),
            BuiltinDerivedType::Short => SCHEMA_BUILTIN_SHORT.clone(),
            BuiltinDerivedType::Token => SCHEMA_BUILTIN_TOKEN.clone(),
            BuiltinDerivedType::UnsignedByte => SCHEMA_BUILTIN_UNSIGNED_BYTE.clone(),
            BuiltinDerivedType::UnsignedInt => SCHEMA_BUILTIN_UNSIGNED_INT.clone(),
            BuiltinDerivedType::UnsignedLong => SCHEMA_BUILTIN_UNSIGNED_LONG.clone(),
            BuiltinDerivedType::UnsignedShort => SCHEMA_BUILTIN_UNSIGNED_SHORT.clone(),
        }
    }
}

/// User-defined derived type.
///
/// It can be generated using [`SimpleTypeDefinitionBuilder`].
#[derive(Clone, PartialEq)]
pub struct UserDefinedType {
    name: Arc<str>,
    target_namespace: Option<Arc<str>>,
    variety: Variety,
    facets: Facets,
    base_type_definition: Arc<SimpleTypeDefinition>,
    r#final: FinalSet,
}

impl UserDefinedType {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn target_namespace(&self) -> Option<&str> {
        self.target_namespace.as_deref()
    }

    /// # Reference
    /// - [4.1.1 The Simple Type Definition Schema Component](https://www.w3.org/TR/xmlschema-2/#dc-defn)
    ///
    /// ```text
    /// {base type definition}
    ///     If the datatype has been derived by restriction then the Simple Type Definition
    ///     component from which it is derived, otherwise the Simple Type Definition for
    ///     anySimpleType (§4.1.6).
    /// ```
    pub fn base_type_definition(&self) -> Arc<SimpleTypeDefinition> {
        self.base_type_definition.clone()
    }

    pub fn variety(&self) -> Variety {
        self.variety.clone()
    }

    /// # Reference
    /// - [4.2.2 ordered](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#rf-ordered)
    ///
    /// ```text
    /// When {variety} is atomic, {value} is inherited from {value} of {base type definition}.
    /// For all primitive types {value} is as specified in the table in Fundamental Facets (§C.1).
    /// When {variety} is list, {value} is false.
    /// When {variety} is union, {value} is partial unless one of the following:
    ///     - If every member of {member type definitions} is derived from a common ancestor other
    ///       than the simple ur-type, then {value} is the same as that ancestor's ordered facet
    ///     - If every member of {member type definitions} has a {value} of false for the ordered
    ///       facet, then {value} is false
    /// ```
    pub fn ordered(&self) -> Ordered {
        match &self.variety() {
            Variety::Atomic(prim) => prim.ordered(),
            Variety::List(_) => Ordered::False,
            Variety::Union(union) => {
                if union.iter().all(|u| u.ordered() == Some(Ordered::False)) {
                    Ordered::False
                } else if union.iter().all(|u| u.ordered() == Some(Ordered::Total)) {
                    // When all member types are totally ordered, the ancestors of each member type are
                    // unique, so it suffices to trace just one ancestor until reaching a Primitive.
                    let mut ancestor = vec![];
                    for u in &**union {
                        let mut now = u.clone();
                        while !matches!(now.as_ref(), SimpleTypeDefinition::AnySimpleType) {
                            match now.variety() {
                                Some(Variety::Atomic(atom)) => {
                                    ancestor.push(atom);
                                    break;
                                }
                                Some(Variety::List(item)) => now = item.clone(),
                                Some(Variety::Union(member)) => now = member[0].clone(),
                                None => return Ordered::Partial,
                            }
                        }
                    }
                    ancestor.dedup();
                    if ancestor.len() == 1 {
                        Ordered::Total
                    } else {
                        Ordered::Partial
                    }
                } else {
                    Ordered::Partial
                }
            }
        }
    }

    /// # Reference
    /// - [4.2.3 bounded](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#rf-bounded)
    ///
    /// ```text
    /// When {variety} is atomic, if one of minInclusive or minExclusive and one of maxInclusive
    /// or maxExclusive are among {facets} , then {value} is true; else {value} is false.
    /// When {variety} is list, if length or both of minLength and maxLength are among {facets},
    /// then {value} is true; else {value} is false.
    /// When {variety} is union, if {value} is true for every member of {member type definitions}
    /// and all members of {member type definitions} share a common ancestor, then {value} is true;
    /// else {value} is false.
    /// ```
    pub fn bounded(&self) -> bool {
        match self.variety() {
            Variety::Atomic(_) => {
                (self.min_inclusive().is_some() || self.min_exclusive().is_some())
                    && (self.max_inclusive().is_some() || self.max_exclusive().is_some())
            }
            Variety::List(_) => {
                self.length().is_some()
                    || (self.min_length().is_some() && self.max_length().is_some())
            }
            Variety::Union(member) => {
                member.iter().all(|m| m.bounded())
                    && !Arc::ptr_eq(&common_ancestor(&member), &SCHEMA_BUILTIN_ANY_SIMPLE_TYPE)
            }
        }
    }

    /// # Reference
    /// - [4.2.4 cardinality](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#rf-cardinality)
    ///
    /// ```text
    /// When {variety} is atomic and {value} of {base type definition} is finite, then {value} is finite.
    /// When {variety} is atomic and {value} of {base type definition} is countably infinite and either
    /// of the following conditions are true, then {value} is finite; else {value} is countably infinite:
    ///     1. one of length, maxLength, totalDigits is among {facets},
    ///     2. all of the following are true:
    ///         a. one of minInclusive or minExclusive is among {facets}
    ///         b. one of maxInclusive or maxExclusive is among {facets}
    ///         c. either of the following are true:
    ///             i.  fractionDigits is among {facets}
    ///             ii. {base type definition} is one of date, gYearMonth, gYear, gMonthDay, gDay
    ///                 or gMonth or any type derived from them
    /// When {variety} is list, if length or both of minLength and maxLength are among {facets},
    /// then {value} is finite; else {value} is countably infinite.
    /// When {variety} is union, if {value} is finite for every member of {member type definitions},
    /// then {value} is finite; else {value} is countably infinite.
    /// ```
    pub fn cardinality(&self) -> Cardinality {
        match self.variety() {
            Variety::Atomic(atom) => {
                if matches!(
                    self.base_type_definition().cardinality(),
                    Cardinality::Finite
                ) {
                    return Cardinality::Finite;
                }

                if self.length().is_some()
                    || self.max_length().is_some()
                    || self.total_digits().is_some()
                {
                    return Cardinality::Finite;
                }

                if (self.min_inclusive().is_none() && self.min_exclusive().is_none())
                    || (self.max_inclusive().is_none() && self.max_exclusive().is_none())
                {
                    return Cardinality::CountablyInfinite;
                }

                if self.fraction_digits().is_some() {
                    return Cardinality::Finite;
                }

                if matches!(
                    atom,
                    BuiltinPrimitiveType::Date
                        | BuiltinPrimitiveType::GYearMonth
                        | BuiltinPrimitiveType::GYear
                        | BuiltinPrimitiveType::GMonthDay
                        | BuiltinPrimitiveType::GDay
                        | BuiltinPrimitiveType::GMonth
                ) {
                    Cardinality::Finite
                } else {
                    Cardinality::CountablyInfinite
                }
            }
            Variety::List(_) => {
                if self.length().is_some()
                    || (self.min_length().is_some() && self.max_length().is_some())
                {
                    Cardinality::Finite
                } else {
                    Cardinality::CountablyInfinite
                }
            }
            Variety::Union(member) => {
                if member
                    .iter()
                    .all(|m| matches!(m.cardinality(), Cardinality::Finite))
                {
                    Cardinality::Finite
                } else {
                    Cardinality::CountablyInfinite
                }
            }
        }
    }

    /// # Reference
    /// - [4.2.5 numeric](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#rf-numeric)
    ///
    /// ```text
    /// When {variety} is atomic, {value} is inherited from {value} of {base type definition}.
    /// For all primitive types {value} is as specified in the table in Fundamental Facets (§C.1).
    /// When {variety} is list, {value} is false.
    /// When {variety} is union, if {value} is true for every member of {member type definitions},
    /// then {value} is true; else {value} is false.
    /// ```
    pub fn numeric(&self) -> bool {
        match self.variety() {
            Variety::Atomic(_) => self.base_type_definition.numeric(),
            Variety::List(_) => false,
            Variety::Union(member) => member.iter().all(|m| m.numeric()),
        }
    }

    pub fn length(&self) -> Option<i32> {
        self.facets
            .length()
            .or_else(|| self.base_type_definition.length())
    }

    pub fn min_length(&self) -> Option<i32> {
        self.facets
            .min_length()
            .or_else(|| self.base_type_definition.min_length())
    }

    pub fn max_length(&self) -> Option<i32> {
        self.facets
            .max_length()
            .or_else(|| self.base_type_definition.max_length())
    }

    /// # Reference
    /// - [4.3.6 whiteSpace](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#rf-whiteSpace)
    ///
    /// # Note
    /// Since the result may not be uniquely determined for union types or types derived
    /// from union types, [`None`] is returned.
    pub fn whitespace(&self) -> Option<Whitespace> {
        match &self.variety {
            Variety::Atomic(_) => self.facets.whitespace,
            Variety::List(_) => Some(Whitespace::Collapse),
            Variety::Union(_) => None,
        }
    }

    pub fn max_inclusive(&self) -> Option<&SchemaValue> {
        self.facets
            .max_inclusive
            .as_ref()
            .or_else(|| self.base_type_definition.max_inclusive())
    }

    pub fn max_exclusive(&self) -> Option<&SchemaValue> {
        self.facets
            .max_exclusive
            .as_ref()
            .or_else(|| self.base_type_definition.max_exclusive())
    }

    pub fn min_inclusive(&self) -> Option<&SchemaValue> {
        self.facets
            .min_inclusive
            .as_ref()
            .or_else(|| self.base_type_definition.min_inclusive())
    }

    pub fn min_exclusive(&self) -> Option<&SchemaValue> {
        self.facets
            .min_exclusive
            .as_ref()
            .or_else(|| self.base_type_definition.min_exclusive())
    }

    pub fn total_digits(&self) -> Option<i32> {
        self.facets
            .total_digits()
            .or_else(|| self.base_type_definition.total_digits())
    }

    pub fn fraction_digits(&self) -> Option<i32> {
        self.facets
            .fraction_digits()
            .or_else(|| self.base_type_definition.fraction_digits())
    }

    pub fn r#final(&self) -> FinalSet {
        self.r#final
    }

    /// # Reference
    /// - [Schema Component Constraint: applicable facets](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#defn-coss)
    pub fn is_applicable_facets(&self, facet: FacetType) -> bool {
        use FacetType::*;

        match self.variety {
            Variety::Atomic(ref atomic) => atomic.is_applicable_facets(facet),
            Variety::List(_) => {
                matches!(
                    facet,
                    Length | MinLength | MaxLength | Pattern | Enumeration | WhiteSpace
                )
            }
            Variety::Union(_) => {
                matches!(facet, Pattern | Enumeration)
            }
        }
    }

    fn check_facet(&self, value: &SchemaValue, literal: &str) -> bool {
        if self.min_exclusive().is_some_and(|min| min >= value)
            || self.min_inclusive().is_some_and(|min| min > value)
            || self.max_exclusive().is_some_and(|max| max <= value)
            || self.max_inclusive().is_some_and(|max| max < value)
            || (!self.facets.enumeration.is_empty()
                && self.facets.enumeration.iter().all(|e| e != value))
        {
            return false;
        }

        match value {
            SchemaValue::QName(_, _) | SchemaValue::NOTATION(_, _) => {
                // [VR: Length Valid]
                // [VR: minLength Valid]
                // [VR: maxLength Valid]
                // For QName and NOTATION, any value for length, minLength, and maxLength is valid.
            }
            v if v.length().is_some() => {
                let length = v.length().unwrap();
                if self.length().is_some_and(|l| length != l as usize)
                    || self.min_length().is_some_and(|l| length < l as usize)
                    || self.max_length().is_some_and(|l| length > l as usize)
                {
                    return false;
                }
            }
            _ => {}
        }

        if let Some(pat) = self.facets.pattern.as_deref()
            && XSRegexp::compile(pat)
                .ok()
                .is_none_or(|pat| !pat.is_match(literal))
        {
            return false;
        }

        if self
            .total_digits()
            .is_some_and(|t| value.total_digits().is_none_or(|td| td > t as usize))
            || self
                .fraction_digits()
                .is_some_and(|f| value.fraction_digits().is_none_or(|fd| fd > f as usize))
        {
            return false;
        }
        true
    }

    pub fn parse(
        &self,
        literal: &str,
        namespaces: &NamespaceStack,
    ) -> Result<SchemaValue, ParseError> {
        let literal = match self.whitespace() {
            Some(Whitespace::Preserve) | None => Cow::Borrowed(literal),
            Some(Whitespace::Replace) => Cow::Owned(
                literal
                    .replace("\x09", "\x20")
                    .replace("\x0A", "\x20")
                    .replace("\x0D", "\x20"),
            ),
            Some(Whitespace::Collapse) => {
                let value = literal
                    .replace("\x09", "\x20")
                    .replace("\x0A", "\x20")
                    .replace("\x0D", "\x20");
                let value = value.trim_matches('\x20');
                let mut buf = String::with_capacity(value.len());
                for c in value.chars() {
                    if c != '\x20' || !buf.ends_with('\x20') {
                        buf.push(c);
                    }
                }
                Cow::Owned(buf)
            }
        };
        match &self.variety {
            Variety::Atomic(_) => {
                let value = self.base_type_definition.parse(&literal, namespaces)?;
                if self.check_facet(&value, &literal) {
                    Ok(value)
                } else {
                    Err(ParseError::Facet)
                }
            }
            Variety::List(item) => {
                let value = SchemaValue::List(
                    literal
                        .split(|c| XMLVersion::default().is_whitespace(c))
                        .filter(|s| !s.is_empty())
                        .map(|s| item.parse(s, namespaces))
                        .collect::<Result<Vec<_>, ParseError>>()?
                        .into(),
                );
                if self.check_facet(&value, &literal) {
                    Ok(value)
                } else {
                    Err(ParseError::Facet)
                }
            }
            Variety::Union(members) => {
                let mut last_error = Err(ParseError::Token);
                for member in members.iter() {
                    match member.parse(&literal, namespaces) {
                        Ok(value) if self.check_facet(&value, &literal) => return Ok(value),
                        Ok(_) => last_error = Err(ParseError::Facet),
                        Err(err) => last_error = Err(err),
                    }
                }
                last_error
            }
        }
    }
}

/// The property 'variety' of the component 'Simple Type Definition'.
///
/// # Reference
/// - [4.1.1 The Simple Type Definition Schema Component](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#dc-defn)
#[derive(Clone, PartialEq)]
pub enum Variety {
    Atomic(BuiltinPrimitiveType),
    List(Arc<SimpleTypeDefinition>),
    Union(Arc<[Arc<SimpleTypeDefinition>]>),
}

/// Fundamental facet 'ordered'.
///
/// # Reference
/// - [4.2.2 ordered](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#rf-ordered)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Ordered {
    False,
    Partial,
    Total,
}

/// Fundamental facet 'cardinality'.
///
/// # Reference
/// - [4.2.4 cardinality](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#rf-cardinality)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Cardinality {
    Finite,
    CountablyInfinite,
}

/// The set of the value of the property 'final' of the component 'Simple Type Definition'.
///
/// # Reference
/// - [4.1.1 The Simple Type Definition Schema Component](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#dc-defn)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct FinalSet(u8);

impl FinalSet {
    pub fn all(&self) -> bool {
        *self == Final::List | Final::Union | Final::Restriction
    }

    pub fn list(&self) -> bool {
        self.0 & Final::List as u8 != 0
    }

    pub fn union(&self) -> bool {
        self.0 & Final::Union as u8 != 0
    }

    pub fn restriction(&self) -> bool {
        self.0 & Final::Restriction as u8 != 0
    }
}

impl std::ops::BitOr<Final> for FinalSet {
    type Output = FinalSet;
    fn bitor(self, rhs: Final) -> Self::Output {
        FinalSet(self.0 | rhs as u8)
    }
}

/// The property 'final' of the component 'Simple Type Definition'.
///
/// # Reference
/// - [4.1.1 The Simple Type Definition Schema Component](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#dc-defn)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Final {
    List = 1,
    Union = 2,
    Restriction = 4,
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

pub struct SimpleTypeDefinitionBuilder {
    base: Arc<SimpleTypeDefinition>,
    name: Option<Arc<str>>,
    target_namespace: Option<Arc<str>>,
    variety: Variety,
    facets: Facets,
    r#final: FinalSet,
}

impl SimpleTypeDefinitionBuilder {
    pub fn build(self) -> Arc<SimpleTypeDefinition> {
        Arc::new(SimpleTypeDefinition::UserDefined(UserDefinedType {
            name: self.name.unwrap_or_default(),
            target_namespace: self.target_namespace,
            variety: self.variety,
            facets: self.facets,
            base_type_definition: self.base,
            r#final: self.r#final,
        }))
    }

    pub fn name(mut self, name: Arc<str>, target_namespace: Option<Arc<str>>) -> Self {
        self.name = Some(name);
        self.target_namespace = target_namespace;
        self
    }

    pub fn length(mut self, length: usize) -> Result<Self, SchemaTypeError> {
        if !self.base.is_applicable_facets(FacetType::Length) {
            return Err(FacetError::Unacceptable.into());
        }
        // [SRC: Single Facet Value]
        if self.facets.length >= 0 {
            return Err(FacetError::Duplicate.into());
        }
        if length > i32::MAX as usize {
            return Err(FacetError::TooLargeLength.into());
        }
        // [SCC: length valid restriction]
        if self.facets.length().is_some_and(|l| l != length as i32) {
            return Err(FacetError::ConflictLength.into());
        }
        // [SCC: length and minLength or maxLength]
        if self.facets.min_length().is_some_and(|l| l > length as i32)
            || self.facets.max_length().is_some_and(|l| l < length as i32)
        {
            return Err(FacetError::ConflictLength.into());
        }
        self.facets.length = length as i32;
        Ok(self)
    }

    pub fn min_length(mut self, min_length: usize) -> Result<Self, SchemaTypeError> {
        if !self.base.is_applicable_facets(FacetType::MinLength) {
            return Err(FacetError::Unacceptable.into());
        }
        // [SRC: Single Facet Value]
        if self.facets.min_length >= 0 {
            return Err(FacetError::Duplicate.into());
        }
        if min_length > i32::MAX as usize {
            return Err(FacetError::TooLargeLength.into());
        }
        // [SCC: minLength <= maxLength]
        if self
            .facets
            .max_length()
            .is_some_and(|l| l < min_length as i32)
        {
            return Err(FacetError::ConflictLength.into());
        }
        // [SCC: minLength valid restriction]
        if self
            .facets
            .min_length()
            .is_some_and(|l| l > min_length as i32)
        {
            return Err(FacetError::ConflictLength.into());
        }
        self.facets.min_length = min_length as i32;
        Ok(self)
    }

    pub fn max_length(mut self, max_length: usize) -> Result<Self, SchemaTypeError> {
        if !self.base.is_applicable_facets(FacetType::MaxLength) {
            return Err(FacetError::Unacceptable.into());
        }
        // [SRC: Single Facet Value]
        if self.facets.max_length >= 0 {
            return Err(FacetError::Duplicate.into());
        }
        if max_length > i32::MAX as usize {
            return Err(FacetError::TooLargeLength.into());
        }
        // [SCC: minLength <= maxLength]
        if self
            .facets
            .max_length()
            .is_some_and(|l| l > max_length as i32)
        {
            return Err(FacetError::ConflictLength.into());
        }
        // [SCC: maxLength valid restriction]
        if self
            .facets
            .max_length()
            .is_some_and(|l| l < max_length as i32)
        {
            return Err(FacetError::ConflictLength.into());
        }
        self.facets.max_length = max_length as i32;
        Ok(self)
    }

    pub fn pattern(mut self, pattern: impl Into<String>) -> Result<Self, SchemaTypeError> {
        if !self.base.is_applicable_facets(FacetType::Pattern) {
            return Err(FacetError::Unacceptable.into());
        }
        let pattern = pattern.into();
        if XSRegexp::compile(&pattern).is_err() {
            return Err(FacetError::InvalidPattern.into());
        }
        if let Some(pat) = self.facets.pattern.as_mut() {
            pat.push('|');
            pat.push_str(&pattern);
        } else {
            self.facets.pattern = Some(pattern);
        }
        Ok(self)
    }

    pub fn enumeration(mut self, value: SchemaValue) -> Result<Self, SchemaTypeError> {
        if !self.base.is_applicable_facets(FacetType::Enumeration) {
            return Err(FacetError::Unacceptable.into());
        }
        self.facets.enumeration.push(value);
        Ok(self)
    }

    pub fn whitespace(mut self, value: Whitespace) -> Result<Self, SchemaTypeError> {
        if !self.base.is_applicable_facets(FacetType::WhiteSpace) {
            return Err(FacetError::Unacceptable.into());
        }
        self.facets.whitespace = Some(value);
        Ok(self)
    }

    pub fn max_inclusive(mut self, value: SchemaValue) -> Result<Self, SchemaTypeError> {
        if !self.base.is_applicable_facets(FacetType::MaxInclusive) {
            return Err(FacetError::Unacceptable.into());
        }
        self.facets.max_inclusive = Some(value);
        Ok(self)
    }

    pub fn min_inclusive(mut self, value: SchemaValue) -> Result<Self, SchemaTypeError> {
        if !self.base.is_applicable_facets(FacetType::MinInclusive) {
            return Err(FacetError::Unacceptable.into());
        }
        self.facets.min_inclusive = Some(value);
        Ok(self)
    }

    pub fn max_exclusive(mut self, value: SchemaValue) -> Result<Self, SchemaTypeError> {
        if !self.base.is_applicable_facets(FacetType::MaxExclusive) {
            return Err(FacetError::Unacceptable.into());
        }
        self.facets.max_exclusive = Some(value);
        Ok(self)
    }

    pub fn min_exclusive(mut self, value: SchemaValue) -> Result<Self, SchemaTypeError> {
        if !self.base.is_applicable_facets(FacetType::MinExclusive) {
            return Err(FacetError::Unacceptable.into());
        }
        self.facets.min_exclusive = Some(value);
        Ok(self)
    }

    pub fn total_digits(mut self, total_digits: NonZeroUsize) -> Result<Self, SchemaTypeError> {
        let total_digits = total_digits.get();
        if !self.base.is_applicable_facets(FacetType::TotalDigits) {
            return Err(FacetError::Unacceptable.into());
        }
        // [SRC: Single Facet Value]
        if self.facets.total_digits >= 0 {
            return Err(FacetError::Duplicate.into());
        }
        if total_digits > i32::MAX as usize {
            return Err(FacetError::TooLargeTotalDigits.into());
        }
        self.facets.total_digits = total_digits as i32;
        Ok(self)
    }

    pub fn fraction_digits(mut self, fraction_digits: usize) -> Result<Self, SchemaTypeError> {
        if !self.base.is_applicable_facets(FacetType::FractionDigits) {
            return Err(FacetError::Unacceptable.into());
        }
        // [SRC: Single Facet Value]
        if self.facets.fraction_digits >= 0 {
            return Err(FacetError::Duplicate.into());
        }
        if fraction_digits > i32::MAX as usize {
            return Err(FacetError::TooLargeFractionDigits.into());
        }
        self.facets.fraction_digits = fraction_digits as i32;
        Ok(self)
    }

    pub fn r#final(mut self, r#final: FinalSet) -> Self {
        self.r#final = r#final;
        self
    }
}

/// Returns a pointer to a predefined built-in type with a name matching `name`.
///
/// `name` must be an NCName.
pub fn find_builtin_type_definition(name: &str) -> Option<Arc<SimpleTypeDefinition>> {
    let ret = match name {
        "string" => SCHEMA_BUILTIN_STRING.clone(),
        "boolean" => SCHEMA_BUILTIN_BOOLEAN.clone(),
        "decimal" => SCHEMA_BUILTIN_DECIMAL.clone(),
        "float" => SCHEMA_BUILTIN_FLOAT.clone(),
        "double" => SCHEMA_BUILTIN_DOUBLE.clone(),
        "duration" => SCHEMA_BUILTIN_DURATION.clone(),
        "dateTime" => SCHEMA_BUILTIN_DATETIME.clone(),
        "time" => SCHEMA_BUILTIN_TIME.clone(),
        "date" => SCHEMA_BUILTIN_DATE.clone(),
        "gYearMonth" => SCHEMA_BUILTIN_GYEARMONTH.clone(),
        "gYear" => SCHEMA_BUILTIN_GYEAR.clone(),
        "gMonthDay" => SCHEMA_BUILTIN_GMONTHDAY.clone(),
        "gDay" => SCHEMA_BUILTIN_GDAY.clone(),
        "gMonth" => SCHEMA_BUILTIN_GMONTH.clone(),
        "hexBinary" => SCHEMA_BUILTIN_HEXBINARY.clone(),
        "base64Binary" => SCHEMA_BUILTIN_BASE64BINARY.clone(),
        "anyURI" => SCHEMA_BUILTIN_ANYURI.clone(),
        "QName" => SCHEMA_BUILTIN_QNAME.clone(),
        "NOTATION" => SCHEMA_BUILTIN_NOTATION.clone(),
        "normalizedString" => SCHEMA_BUILTIN_NORMALIZED_STRING.clone(),
        "token" => SCHEMA_BUILTIN_TOKEN.clone(),
        "language" => SCHEMA_BUILTIN_LANGUAGE.clone(),
        "NMTOKEN" => SCHEMA_BUILTIN_NMTOKEN.clone(),
        "NMTOKENS" => SCHEMA_BUILTIN_NMTOKENS.clone(),
        "Name" => SCHEMA_BUILTIN_NAME.clone(),
        "NCName" => SCHEMA_BUILTIN_NCNAME.clone(),
        "ID" => SCHEMA_BUILTIN_ID.clone(),
        "IDREF" => SCHEMA_BUILTIN_IDREF.clone(),
        "IDREFS" => SCHEMA_BUILTIN_IDREFS.clone(),
        "ENTITY" => SCHEMA_BUILTIN_ENTITY.clone(),
        "ENTITIES" => SCHEMA_BUILTIN_ENTITIES.clone(),
        "integer" => SCHEMA_BUILTIN_INTEGER.clone(),
        "nonPositiveInteger" => SCHEMA_BUILTIN_NONPOSITIVE_INTEGER.clone(),
        "negativeInteger" => SCHEMA_BUILTIN_NEGATIVE_INTEGER.clone(),
        "long" => SCHEMA_BUILTIN_LONG.clone(),
        "int" => SCHEMA_BUILTIN_INT.clone(),
        "short" => SCHEMA_BUILTIN_SHORT.clone(),
        "byte" => SCHEMA_BUILTIN_BYTE.clone(),
        "nonNegativeInteger" => SCHEMA_BUILTIN_NONNEGATIVE_INTEGER.clone(),
        "unsignedLong" => SCHEMA_BUILTIN_UNSIGNED_LONG.clone(),
        "unsignedInt" => SCHEMA_BUILTIN_UNSIGNED_INT.clone(),
        "unsignedShort" => SCHEMA_BUILTIN_UNSIGNED_SHORT.clone(),
        "unsignedByte" => SCHEMA_BUILTIN_UNSIGNED_BYTE.clone(),
        "positiveInteger" => SCHEMA_BUILTIN_POSITIVE_INTEGER.clone(),
        "anySimpleType" => SCHEMA_BUILTIN_ANY_SIMPLE_TYPE.clone(),
        _ => return None,
    };

    Some(ret)
}

#[cfg(test)]
mod tests {
    use super::*;
    use BuiltinDerivedType::*;
    use BuiltinPrimitiveType::*;

    #[test]
    fn boolean_parse_tests() {
        assert_eq!(
            Boolean.parse("true", &NamespaceStack::default()),
            Ok(SchemaValue::Boolean(true))
        );
        assert_eq!(
            Boolean.parse("false", &NamespaceStack::default()),
            Ok(SchemaValue::Boolean(false))
        );
        assert_eq!(
            Boolean.parse("1", &NamespaceStack::default()),
            Ok(SchemaValue::Boolean(true))
        );
        assert_eq!(
            Boolean.parse("0", &NamespaceStack::default()),
            Ok(SchemaValue::Boolean(false))
        );
    }

    #[test]
    fn decimal_parse_tests() {
        assert_eq!(
            Decimal.parse("-1.23", &NamespaceStack::default()),
            Ok(SchemaValue::Decimal("-1".into(), "23".into()))
        );
        assert_eq!(
            Decimal.parse("12678967.543233", &NamespaceStack::default()),
            Ok(SchemaValue::Decimal("12678967".into(), "543233".into()))
        );
        assert_eq!(
            Decimal.parse("+100000.00", &NamespaceStack::default()),
            Ok(SchemaValue::Decimal("100000".into(), "0".into()))
        );
        assert_eq!(
            Decimal.parse("210", &NamespaceStack::default()),
            Ok(SchemaValue::Decimal("210".into(), "0".into()))
        );
    }

    #[test]
    fn float_parse_tests() {
        assert_eq!(
            Float.parse("-1E4", &NamespaceStack::default()),
            Ok(SchemaValue::Float(-1E4))
        );
        assert_eq!(
            Float.parse("1267.43233E12", &NamespaceStack::default()),
            Ok(SchemaValue::Float(1.267_432_4E15))
        );
        assert_eq!(
            Float.parse("12.78e-2", &NamespaceStack::default()),
            Ok(SchemaValue::Float(12.78e-2))
        );
        assert_eq!(
            Float.parse("-0", &NamespaceStack::default()),
            Ok(SchemaValue::Float(-0.))
        );
        assert_eq!(
            Float.parse("0", &NamespaceStack::default()),
            Ok(SchemaValue::Float(0.))
        );
        assert_eq!(
            Float.parse("INF", &NamespaceStack::default()),
            Ok(SchemaValue::Float(f32::INFINITY))
        );
    }

    #[test]
    fn double_parse_tests() {
        assert_eq!(
            Double.parse("-1E4", &NamespaceStack::default()),
            Ok(SchemaValue::Double(-1E4))
        );
        assert_eq!(
            Double.parse("1267.43233E12", &NamespaceStack::default()),
            Ok(SchemaValue::Double(1267.43233E12))
        );
        assert_eq!(
            Double.parse("12.78e-2", &NamespaceStack::default()),
            Ok(SchemaValue::Double(12.78e-2))
        );
        assert_eq!(
            Double.parse("-0", &NamespaceStack::default()),
            Ok(SchemaValue::Double(-0.))
        );
        assert_eq!(
            Double.parse("0", &NamespaceStack::default()),
            Ok(SchemaValue::Double(0.))
        );
        assert_eq!(
            Double.parse("INF", &NamespaceStack::default()),
            Ok(SchemaValue::Double(f64::INFINITY))
        );
    }

    #[test]
    fn duration_parse_tests() {
        assert!(Duration.parse("P1347Y", &NamespaceStack::default()).is_ok());
        assert!(Duration.parse("P1347M", &NamespaceStack::default()).is_ok());
        assert!(
            Duration
                .parse("P1Y2MT2H", &NamespaceStack::default())
                .is_ok()
        );
        assert!(
            Duration
                .parse("P0Y1347M", &NamespaceStack::default())
                .is_ok()
        );
        assert!(
            Duration
                .parse("P0Y1347M0D", &NamespaceStack::default())
                .is_ok()
        );
        assert!(
            Duration
                .parse("P-1347M", &NamespaceStack::default())
                .is_err()
        );
        assert!(
            Duration
                .parse("-P1347M", &NamespaceStack::default())
                .is_ok()
        );
        assert!(
            Duration
                .parse("P1Y2MT", &NamespaceStack::default())
                .is_err()
        );
    }

    #[test]
    fn datetime_parse_tests() {
        assert!(
            DateTime
                .parse("2002-10-10T12:00:00-05:00", &NamespaceStack::default())
                .is_ok()
        );
        assert!(
            DateTime
                .parse("2002-10-10T17:00:00Z", &NamespaceStack::default())
                .is_ok()
        );
        assert!(
            DateTime
                .parse("2002-10-10T12:00:00Z", &NamespaceStack::default())
                .is_ok()
        );
    }

    #[test]
    fn gyearmonth_parse_tests() {
        assert!(
            GYearMonth
                .parse("1999-05", &NamespaceStack::default())
                .is_ok()
        );
    }

    #[test]
    fn gyear_parse_tests() {
        assert!(GYear.parse("1999", &NamespaceStack::default()).is_ok());
    }

    #[test]
    fn hexbinary_parse_tests() {
        assert_eq!(
            HexBinary.parse("0FB7", &NamespaceStack::default()),
            Ok(SchemaValue::HexBinary("0FB7".into()))
        );
    }

    #[test]
    fn integer_parse_tests() {
        assert_eq!(
            Integer.parse("-1", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(-1))
        );
        assert_eq!(
            Integer.parse("0", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(0))
        );
        assert_eq!(
            Integer.parse("2678967543233", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(2678967543233))
        );
        assert_eq!(
            Integer.parse("+100000", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(100000))
        );
    }

    #[test]
    fn non_positive_integer_parse_tests() {
        assert_eq!(
            NonPositiveInteger.parse("-1", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(-1))
        );
        assert_eq!(
            NonPositiveInteger.parse("0", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(0))
        );
        assert_eq!(
            NonPositiveInteger.parse("-12678967543233", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(-12678967543233))
        );
        assert_eq!(
            NonPositiveInteger.parse("-100000", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(-100000))
        );
    }

    #[test]
    fn negative_integer_parse_tests() {
        assert_eq!(
            NegativeInteger.parse("-1", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(-1))
        );
        assert_eq!(
            NegativeInteger.parse("-12678967543233", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(-12678967543233))
        );
        assert_eq!(
            NegativeInteger.parse("-100000", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(-100000))
        );
    }

    #[test]
    fn long_parse_tests() {
        assert_eq!(
            Long.parse("-1", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(-1))
        );
        assert_eq!(
            Long.parse("0", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(0))
        );
        assert_eq!(
            Long.parse("12678967543233", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(12678967543233))
        );
        assert_eq!(
            Long.parse("+100000", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(100000))
        );
    }

    #[test]
    fn int_parse_tests() {
        assert_eq!(
            Int.parse("-1", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(-1))
        );
        assert_eq!(
            Int.parse("0", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(0))
        );
        assert_eq!(
            Int.parse("126789675", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(126789675))
        );
        assert_eq!(
            Int.parse("+100000", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(100000))
        );
    }

    #[test]
    fn short_parse_tests() {
        assert_eq!(
            Short.parse("-1", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(-1))
        );
        assert_eq!(
            Short.parse("0", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(0))
        );
        assert_eq!(
            Short.parse("12678", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(12678))
        );
        assert_eq!(
            Short.parse("+10000", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(10000))
        );
    }

    #[test]
    fn byte_parse_tests() {
        assert_eq!(
            Short.parse("-1", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(-1))
        );
        assert_eq!(
            Short.parse("0", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(0))
        );
        assert_eq!(
            Short.parse("126", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(126))
        );
        assert_eq!(
            Short.parse("+100", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(100))
        );
    }

    #[test]
    fn non_negative_integer_parse_tests() {
        assert_eq!(
            NonNegativeInteger.parse("1", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(1))
        );
        assert_eq!(
            NonNegativeInteger.parse("0", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(0))
        );
        assert_eq!(
            NonNegativeInteger.parse("12678967543233", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(12678967543233))
        );
        assert_eq!(
            NonNegativeInteger.parse("+100000", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(100000))
        );
    }

    #[test]
    fn unsigned_long_parse_tests() {
        assert_eq!(
            UnsignedLong.parse("0", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(0))
        );
        assert_eq!(
            UnsignedLong.parse("12678967543233", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(12678967543233))
        );
        assert_eq!(
            UnsignedLong.parse("100000", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(100000))
        );
    }

    #[test]
    fn unsigned_int_parse_tests() {
        assert_eq!(
            UnsignedInt.parse("0", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(0))
        );
        assert_eq!(
            UnsignedInt.parse("1267896754", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(1267896754))
        );
        assert_eq!(
            UnsignedInt.parse("100000", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(100000))
        );
    }

    #[test]
    fn unsigned_short_parse_tests() {
        assert_eq!(
            UnsignedShort.parse("0", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(0))
        );
        assert_eq!(
            UnsignedShort.parse("12678", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(12678))
        );
        assert_eq!(
            UnsignedShort.parse("10000", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(10000))
        );
    }

    #[test]
    fn unsigned_byte_parse_tests() {
        assert_eq!(
            UnsignedShort.parse("0", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(0))
        );
        assert_eq!(
            UnsignedShort.parse("126", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(126))
        );
        assert_eq!(
            UnsignedShort.parse("100", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(100))
        );
    }

    #[test]
    fn positive_integer_parse_tests() {
        assert_eq!(
            PositiveInteger.parse("1", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(1))
        );
        assert_eq!(
            PositiveInteger.parse("12678967543233", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(12678967543233))
        );
        assert_eq!(
            PositiveInteger.parse("+100000", &NamespaceStack::default()),
            Ok(SchemaValue::Integer(100000))
        );
    }
}
