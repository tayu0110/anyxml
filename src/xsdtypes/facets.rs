use std::str::FromStr;

use crate::xsdtypes::{ParseError, SchemaTypeError, SchemaValue};

/// # Reference
/// - [4.3 Constraining Facets](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#rf-facets)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FacetType {
    Length,
    MinLength,
    MaxLength,
    Pattern,
    Enumeration,
    WhiteSpace,
    MaxInclusive,
    MaxExclusive,
    MinExclusive,
    MinInclusive,
    TotalDigits,
    FractionDigits,
}

impl FromStr for FacetType {
    type Err = SchemaTypeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "length" => Ok(FacetType::Length),
            "minLength" => Ok(FacetType::MinLength),
            "maxLength" => Ok(FacetType::MaxLength),
            "pattern" => Ok(FacetType::Pattern),
            "enumeration" => Ok(FacetType::Enumeration),
            "whiteSpace" => Ok(FacetType::WhiteSpace),
            "maxInclusive" => Ok(FacetType::MaxInclusive),
            "maxExclusive" => Ok(FacetType::MaxExclusive),
            "minExclusive" => Ok(FacetType::MinExclusive),
            "minInclusive" => Ok(FacetType::MinInclusive),
            "totalDigits" => Ok(FacetType::TotalDigits),
            "fractionDigits" => Ok(FacetType::FractionDigits),
            _ => Err(SchemaTypeError::ParseError(ParseError::Facet)),
        }
    }
}

#[derive(Clone, PartialEq)]
pub(super) struct Facets {
    /// if i32::MIN, not set.
    /// if negative, parent type is restricted by its absolute value
    pub(super) length: i32,
    /// if i32::MIN, not set.
    /// if negative, parent type is restricted by its absolute value
    pub(super) min_length: i32,
    /// if i32::MIN, not set.
    /// if negative, parent type is restricted by its absolute value
    pub(super) max_length: i32,
    pub(super) pattern: Option<String>,
    pub(super) enumeration: Vec<SchemaValue>,
    pub(super) whitespace: Option<Whitespace>,
    pub(super) max_inclusive: Option<SchemaValue>,
    pub(super) max_exclusive: Option<SchemaValue>,
    pub(super) min_exclusive: Option<SchemaValue>,
    pub(super) min_inclusive: Option<SchemaValue>,
    /// if i32::MIN, not set.
    /// if negative, parent type is restricted by its absolute value
    pub(super) total_digits: i32,
    /// if i32::MIN, not set.
    /// if negative, parent type is restricted by its absolute value
    pub(super) fraction_digits: i32,
}

impl Facets {
    pub(super) fn derive(&self) -> Self {
        Facets {
            length: self.length().map_or(i32::MIN, |l| -l),
            min_length: self.min_length().map_or(i32::MIN, |l| -l),
            max_length: self.max_length().map_or(i32::MIN, |l| -l),
            pattern: None,
            enumeration: vec![],
            whitespace: self.whitespace,
            max_inclusive: None,
            max_exclusive: None,
            min_exclusive: None,
            min_inclusive: None,
            total_digits: self.total_digits().map_or(i32::MIN, |l| -l),
            fraction_digits: self.fraction_digits().map_or(i32::MIN, |l| -l),
        }
    }

    pub(super) fn length(&self) -> Option<i32> {
        (self.length != i32::MIN).then(|| self.length.abs())
    }

    pub(super) fn min_length(&self) -> Option<i32> {
        (self.min_length != i32::MIN).then(|| self.min_length.abs())
    }

    pub(super) fn max_length(&self) -> Option<i32> {
        (self.max_length != i32::MIN).then(|| self.max_length.abs())
    }

    pub(super) fn total_digits(&self) -> Option<i32> {
        (self.total_digits != i32::MIN).then(|| self.total_digits.abs())
    }

    pub(super) fn fraction_digits(&self) -> Option<i32> {
        (self.fraction_digits != i32::MIN).then(|| self.fraction_digits.abs())
    }
}

impl Default for Facets {
    fn default() -> Self {
        Facets {
            length: i32::MIN,
            min_length: i32::MIN,
            max_length: i32::MIN,
            pattern: None,
            enumeration: vec![],
            whitespace: None,
            max_inclusive: None,
            max_exclusive: None,
            min_exclusive: None,
            min_inclusive: None,
            total_digits: i32::MIN,
            fraction_digits: i32::MIN,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FacetError {
    TooLargeLength,
    TooLargeTotalDigits,
    TooLargeFractionDigits,
    ConflictLength,
    Duplicate,
    InvalidPattern,
    Unacceptable,
}

/// # Reference
/// - [4.3.6 whiteSpace](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#rf-whiteSpace)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Whitespace {
    Preserve,
    Replace,
    Collapse,
}
