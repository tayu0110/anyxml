use std::sync::{Arc, LazyLock};

use crate::xsdtypes::{BuiltinDerivedType, BuiltinPrimitiveType, SimpleTypeDefinition};

/// [anySimpleType](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#anySimpleType-component).
pub static SCHEMA_BUILTIN_ANY_SIMPLE_TYPE: LazyLock<Arc<SimpleTypeDefinition>> =
    LazyLock::new(|| Arc::new(SimpleTypeDefinition::AnySimpleType));

/// [string](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#string).
pub static SCHEMA_BUILTIN_STRING: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinPrimitive(
        BuiltinPrimitiveType::String,
    ))
});
/// [boolean](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#boolean).
pub static SCHEMA_BUILTIN_BOOLEAN: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinPrimitive(
        BuiltinPrimitiveType::Boolean,
    ))
});
/// [decimal](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#decimal).
pub static SCHEMA_BUILTIN_DECIMAL: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinPrimitive(
        BuiltinPrimitiveType::Decimal,
    ))
});
/// [float](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#float).
pub static SCHEMA_BUILTIN_FLOAT: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinPrimitive(
        BuiltinPrimitiveType::Float,
    ))
});
/// [double](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#double).
pub static SCHEMA_BUILTIN_DOUBLE: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinPrimitive(
        BuiltinPrimitiveType::Double,
    ))
});
/// [duration](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#duration).
pub static SCHEMA_BUILTIN_DURATION: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinPrimitive(
        BuiltinPrimitiveType::Duration,
    ))
});
/// [dateTime](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#dateTime).
pub static SCHEMA_BUILTIN_DATETIME: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinPrimitive(
        BuiltinPrimitiveType::DateTime,
    ))
});
/// [time](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#time).
pub static SCHEMA_BUILTIN_TIME: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinPrimitive(
        BuiltinPrimitiveType::Time,
    ))
});
/// [date](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#date).
pub static SCHEMA_BUILTIN_DATE: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinPrimitive(
        BuiltinPrimitiveType::Date,
    ))
});
/// [gYearMonth](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#gYearMonth).
pub static SCHEMA_BUILTIN_GYEARMONTH: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinPrimitive(
        BuiltinPrimitiveType::GYearMonth,
    ))
});
/// [gYear](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#gYear).
pub static SCHEMA_BUILTIN_GYEAR: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinPrimitive(
        BuiltinPrimitiveType::GYear,
    ))
});
/// [gMonthDay](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#gMonthDay).
pub static SCHEMA_BUILTIN_GMONTHDAY: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinPrimitive(
        BuiltinPrimitiveType::GMonthDay,
    ))
});
/// [gDay](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#gDay).
pub static SCHEMA_BUILTIN_GDAY: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinPrimitive(
        BuiltinPrimitiveType::GDay,
    ))
});
/// [gMonth](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#gMonth).
pub static SCHEMA_BUILTIN_GMONTH: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinPrimitive(
        BuiltinPrimitiveType::GMonth,
    ))
});
/// [hexBinary](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#hexBinary).
pub static SCHEMA_BUILTIN_HEXBINARY: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinPrimitive(
        BuiltinPrimitiveType::HexBinary,
    ))
});
/// [base64Binary](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#base64Binary).
pub static SCHEMA_BUILTIN_BASE64BINARY: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinPrimitive(
        BuiltinPrimitiveType::Base64Binary,
    ))
});
/// [anyURI](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#anyURI).
pub static SCHEMA_BUILTIN_ANYURI: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinPrimitive(
        BuiltinPrimitiveType::AnyURI,
    ))
});
/// [QName](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#QName).
pub static SCHEMA_BUILTIN_QNAME: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinPrimitive(
        BuiltinPrimitiveType::QName,
    ))
});
/// [NOTATION](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#NOTATION).
pub static SCHEMA_BUILTIN_NOTATION: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinPrimitive(
        BuiltinPrimitiveType::NOTATION,
    ))
});

/// [normalizedString](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#normalizedString).
pub static SCHEMA_BUILTIN_NORMALIZED_STRING: LazyLock<Arc<SimpleTypeDefinition>> =
    LazyLock::new(|| {
        Arc::new(SimpleTypeDefinition::BuiltinDerived(
            BuiltinDerivedType::NormalizedString,
        ))
    });
/// [token](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#token).
pub static SCHEMA_BUILTIN_TOKEN: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinDerived(
        BuiltinDerivedType::Token,
    ))
});
/// [language](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#language).
pub static SCHEMA_BUILTIN_LANGUAGE: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinDerived(
        BuiltinDerivedType::Language,
    ))
});
/// [NMTOKEN](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#NMTOKEN).
pub static SCHEMA_BUILTIN_NMTOKEN: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinDerived(
        BuiltinDerivedType::NMTOKEN,
    ))
});
/// [NMTOKENS](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#NMTOKENS).
pub static SCHEMA_BUILTIN_NMTOKENS: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinDerived(
        BuiltinDerivedType::NMTOKENS,
    ))
});
/// [Name](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#Name).
pub static SCHEMA_BUILTIN_NAME: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinDerived(
        BuiltinDerivedType::Name,
    ))
});
/// [NCName](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#NCName).
pub static SCHEMA_BUILTIN_NCNAME: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinDerived(
        BuiltinDerivedType::NCName,
    ))
});
/// [ID](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#ID).
pub static SCHEMA_BUILTIN_ID: LazyLock<Arc<SimpleTypeDefinition>> =
    LazyLock::new(|| Arc::new(SimpleTypeDefinition::BuiltinDerived(BuiltinDerivedType::ID)));
/// [IDREF](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#IDREF).
pub static SCHEMA_BUILTIN_IDREF: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinDerived(
        BuiltinDerivedType::IDREF,
    ))
});
/// [IDREFS](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#IDREFS).
pub static SCHEMA_BUILTIN_IDREFS: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinDerived(
        BuiltinDerivedType::IDREFS,
    ))
});
/// [ENTITY](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#ENTITY).
pub static SCHEMA_BUILTIN_ENTITY: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinDerived(
        BuiltinDerivedType::ENTITY,
    ))
});
/// [ENTITIES](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#ENTITIES).
pub static SCHEMA_BUILTIN_ENTITIES: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinDerived(
        BuiltinDerivedType::ENTITIES,
    ))
});
/// [integer](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#integer).
pub static SCHEMA_BUILTIN_INTEGER: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinDerived(
        BuiltinDerivedType::Integer,
    ))
});
/// [nonPositiveInteger](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#nonPositiveInteger).
pub static SCHEMA_BUILTIN_NONPOSITIVE_INTEGER: LazyLock<Arc<SimpleTypeDefinition>> =
    LazyLock::new(|| {
        Arc::new(SimpleTypeDefinition::BuiltinDerived(
            BuiltinDerivedType::NonPositiveInteger,
        ))
    });
/// [negativeInteger](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#negativeInteger).
pub static SCHEMA_BUILTIN_NEGATIVE_INTEGER: LazyLock<Arc<SimpleTypeDefinition>> =
    LazyLock::new(|| {
        Arc::new(SimpleTypeDefinition::BuiltinDerived(
            BuiltinDerivedType::NegativeInteger,
        ))
    });
/// [long](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#long).
pub static SCHEMA_BUILTIN_LONG: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinDerived(
        BuiltinDerivedType::Long,
    ))
});
/// [int](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#int).
pub static SCHEMA_BUILTIN_INT: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinDerived(
        BuiltinDerivedType::Int,
    ))
});
/// [short](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#short).
pub static SCHEMA_BUILTIN_SHORT: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinDerived(
        BuiltinDerivedType::Short,
    ))
});
/// [byte](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#byte).
pub static SCHEMA_BUILTIN_BYTE: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinDerived(
        BuiltinDerivedType::Byte,
    ))
});
/// [nonNegativeInteger](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#nonNegativeInteger).
pub static SCHEMA_BUILTIN_NONNEGATIVE_INTEGER: LazyLock<Arc<SimpleTypeDefinition>> =
    LazyLock::new(|| {
        Arc::new(SimpleTypeDefinition::BuiltinDerived(
            BuiltinDerivedType::NonNegativeInteger,
        ))
    });
/// [unsignedLong](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#unsignedLong).
pub static SCHEMA_BUILTIN_UNSIGNED_LONG: LazyLock<Arc<SimpleTypeDefinition>> =
    LazyLock::new(|| {
        Arc::new(SimpleTypeDefinition::BuiltinDerived(
            BuiltinDerivedType::UnsignedLong,
        ))
    });
/// [unsignedInt](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#unsignedInt).
pub static SCHEMA_BUILTIN_UNSIGNED_INT: LazyLock<Arc<SimpleTypeDefinition>> = LazyLock::new(|| {
    Arc::new(SimpleTypeDefinition::BuiltinDerived(
        BuiltinDerivedType::UnsignedInt,
    ))
});
/// [unsignedShort](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#unsignedShort).
pub static SCHEMA_BUILTIN_UNSIGNED_SHORT: LazyLock<Arc<SimpleTypeDefinition>> =
    LazyLock::new(|| {
        Arc::new(SimpleTypeDefinition::BuiltinDerived(
            BuiltinDerivedType::UnsignedShort,
        ))
    });
/// [unsignedByte](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#unsignedByte).
pub static SCHEMA_BUILTIN_UNSIGNED_BYTE: LazyLock<Arc<SimpleTypeDefinition>> =
    LazyLock::new(|| {
        Arc::new(SimpleTypeDefinition::BuiltinDerived(
            BuiltinDerivedType::UnsignedByte,
        ))
    });
/// [positiveInteger](https://www.w3.org/TR/2004/REC-xmlschema-2-20041028/#positiveInteger).
pub static SCHEMA_BUILTIN_POSITIVE_INTEGER: LazyLock<Arc<SimpleTypeDefinition>> =
    LazyLock::new(|| {
        Arc::new(SimpleTypeDefinition::BuiltinDerived(
            BuiltinDerivedType::PositiveInteger,
        ))
    });
