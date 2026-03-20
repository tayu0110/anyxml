use std::sync::{Arc, LazyLock};

use crate::xsdtypes::{BuiltinDerivedType, BuiltinPrimitiveType, SimpleTypeDefinition};

pub static SCHEMA_BUILTIN_ANY_SIMPLE_TYPE: LazyLock<Arc<SimpleTypeDefinition>> =
    LazyLock::new(|| Arc::new(SimpleTypeDefinition::AnySimpleType));

pub static SCHEMA_BUILTIN_STRING: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinPrimitive(
        BuiltinPrimitiveType::String,
    ))
});
pub static SCHEMA_BUILTIN_BOOLEAN: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinPrimitive(
        BuiltinPrimitiveType::Boolean,
    ))
});
pub static SCHEMA_BUILTIN_DECIMAL: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinPrimitive(
        BuiltinPrimitiveType::Decimal,
    ))
});
pub static SCHEMA_BUILTIN_FLOAT: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinPrimitive(
        BuiltinPrimitiveType::Float,
    ))
});
pub static SCHEMA_BUILTIN_DOUBLE: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinPrimitive(
        BuiltinPrimitiveType::Double,
    ))
});
pub static SCHEMA_BUILTIN_DURATION: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinPrimitive(
        BuiltinPrimitiveType::Duration,
    ))
});
pub static SCHEMA_BUILTIN_DATETIME: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinPrimitive(
        BuiltinPrimitiveType::DateTime,
    ))
});
pub static SCHEMA_BUILTIN_TIME: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinPrimitive(
        BuiltinPrimitiveType::Time,
    ))
});
pub static SCHEMA_BUILTIN_DATE: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinPrimitive(
        BuiltinPrimitiveType::Date,
    ))
});
pub static SCHEMA_BUILTIN_GYEARMONTH: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinPrimitive(
        BuiltinPrimitiveType::GYearMonth,
    ))
});
pub static SCHEMA_BUILTIN_GYEAR: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinPrimitive(
        BuiltinPrimitiveType::GYear,
    ))
});
pub static SCHEMA_BUILTIN_GMONTHDAY: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinPrimitive(
        BuiltinPrimitiveType::GMonthDay,
    ))
});
pub static SCHEMA_BUILTIN_GDAY: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinPrimitive(
        BuiltinPrimitiveType::GDay,
    ))
});
pub static SCHEMA_BUILTIN_GMONTH: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinPrimitive(
        BuiltinPrimitiveType::GMonth,
    ))
});
pub static SCHEMA_BUILTIN_HEXBINARY: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinPrimitive(
        BuiltinPrimitiveType::HexBinary,
    ))
});
pub static SCHEMA_BUILTIN_BASE64BINARY: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinPrimitive(
        BuiltinPrimitiveType::Base64Binary,
    ))
});
pub static SCHEMA_BUILTIN_ANYURI: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinPrimitive(
        BuiltinPrimitiveType::AnyURI,
    ))
});
pub static SCHEMA_BUILTIN_QNAME: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinPrimitive(
        BuiltinPrimitiveType::QName,
    ))
});
pub static SCHEMA_BUILTIN_NOTATION: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinPrimitive(
        BuiltinPrimitiveType::NOTATION,
    ))
});

pub static SCHEMA_BUILTIN_NORMALIZED_STRING: LazyLock<Arc<SimpleTypeDefinition>> =
    LazyLock::new(|| {
        Arc::new(SimpleTypeDefinition::BuiltinDerived(
            BuiltinDerivedType::NormalizedString,
        ))
    });
pub static SCHEMA_BUILTIN_TOKEN: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinDerived(
        BuiltinDerivedType::Token,
    ))
});
pub static SCHEMA_BUILTIN_LANGUAGE: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinDerived(
        BuiltinDerivedType::Language,
    ))
});
pub static SCHEMA_BUILTIN_NMTOKEN: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinDerived(
        BuiltinDerivedType::NMTOKEN,
    ))
});
pub static SCHEMA_BUILTIN_NMTOKENS: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinDerived(
        BuiltinDerivedType::NMTOKENS,
    ))
});
pub static SCHEMA_BUILTIN_NAME: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinDerived(
        BuiltinDerivedType::Name,
    ))
});
pub static SCHEMA_BUILTIN_NCNAME: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinDerived(
        BuiltinDerivedType::NCName,
    ))
});
pub static SCHEMA_BUILTIN_ID: LazyLock<Arc<SimpleTypeDefinition>> =
    LazyLock::new(|| Arc::new(SimpleTypeDefinition::BuiltinDerived(BuiltinDerivedType::ID)));
pub static SCHEMA_BUILTIN_IDREF: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinDerived(
        BuiltinDerivedType::IDREF,
    ))
});
pub static SCHEMA_BUILTIN_IDREFS: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinDerived(
        BuiltinDerivedType::IDREFS,
    ))
});
pub static SCHEMA_BUILTIN_ENTITY: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinDerived(
        BuiltinDerivedType::ENTITY,
    ))
});
pub static SCHEMA_BUILTIN_ENTITIES: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinDerived(
        BuiltinDerivedType::ENTITIES,
    ))
});
pub static SCHEMA_BUILTIN_INTEGER: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinDerived(
        BuiltinDerivedType::Integer,
    ))
});
pub static SCHEMA_BUILTIN_NONPOSITIVE_INTEGER: LazyLock<Arc<SimpleTypeDefinition>> =
    LazyLock::new(|| {
        Arc::new(SimpleTypeDefinition::BuiltinDerived(
            BuiltinDerivedType::NonPositiveInteger,
        ))
    });
pub static SCHEMA_BUILTIN_NEGATIVE_INTEGER: LazyLock<Arc<SimpleTypeDefinition>> =
    LazyLock::new(|| {
        Arc::new(SimpleTypeDefinition::BuiltinDerived(
            BuiltinDerivedType::NegativeInteger,
        ))
    });
pub static SCHEMA_BUILTIN_LONG: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinDerived(
        BuiltinDerivedType::Long,
    ))
});
pub static SCHEMA_BUILTIN_INT: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinDerived(
        BuiltinDerivedType::Int,
    ))
});
pub static SCHEMA_BUILTIN_SHORT: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinDerived(
        BuiltinDerivedType::Short,
    ))
});
pub static SCHEMA_BUILTIN_BYTE: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinDerived(
        BuiltinDerivedType::Byte,
    ))
});
pub static SCHEMA_BUILTIN_NONNEGATIVE_INTEGER: LazyLock<Arc<SimpleTypeDefinition>> =
    LazyLock::new(|| {
        Arc::new(SimpleTypeDefinition::BuiltinDerived(
            BuiltinDerivedType::NonNegativeInteger,
        ))
    });
pub static SCHEMA_BUILTIN_UNSIGNED_LONG: LazyLock<Arc<SimpleTypeDefinition>> =
    LazyLock::new(|| {
        Arc::new(SimpleTypeDefinition::BuiltinDerived(
            BuiltinDerivedType::UnsignedLong,
        ))
    });
pub static SCHEMA_BUILTIN_UNSIGNED_INT: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinDerived(
        BuiltinDerivedType::UnsignedInt,
    ))
});
pub static SCHEMA_BUILTIN_UNSIGNED_SHORT: LazyLock<Arc<SimpleTypeDefinition>> =
    LazyLock::new(|| {
        Arc::new(SimpleTypeDefinition::BuiltinDerived(
            BuiltinDerivedType::UnsignedShort,
        ))
    });
pub static SCHEMA_BUILTIN_UNSIGNED_BYTE: LazyLock<Arc<SimpleTypeDefinition>> =
    LazyLock::new(|| {
        Arc::new(SimpleTypeDefinition::BuiltinDerived(
            BuiltinDerivedType::UnsignedByte,
        ))
    });
pub static SCHEMA_BUILTIN_POSITIVE_INTEGER: LazyLock<Arc<SimpleTypeDefinition>> =
    LazyLock::new(|| {
        Arc::new(SimpleTypeDefinition::BuiltinDerived(
            BuiltinDerivedType::PositiveInteger,
        ))
    });
