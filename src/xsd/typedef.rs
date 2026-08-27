use std::{
    collections::{HashMap, HashSet},
    num::NonZeroUsize,
    sync::{Arc, LazyLock},
};

use crate::{
    automata::xsregexp::XSRegexp,
    xsd::{
        XML_SCHEMA_NAMESPACE, XSDParseError,
        parse::{AttributeUse, Particle, Wildcard},
    },
};

static ANY_SIMPLE_TYPE: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let xsd_ns: Arc<str> = Arc::from(XML_SCHEMA_NAMESPACE);
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: Arc::from("anySimpleType"),
        target_namespace: Some(xsd_ns.clone()),
        base_type_definition: (Some(xsd_ns), Arc::from("anyType")),
        facets: Facets::default(),
        fundamental_facets: FundamentalFacets::default(),
        r#final: FinalSet::default(),
        variety: None,
    })
});
static STRING: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("string");
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: name.clone(),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), ANY_SIMPLE_TYPE.name()),
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
        variety: Some(Variety::Atomic(ANY_SIMPLE_TYPE.target_namespace(), name)),
    })
});
static BOOLEAN: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("boolean");
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: name.clone(),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), ANY_SIMPLE_TYPE.name()),
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
        variety: Some(Variety::Atomic(ANY_SIMPLE_TYPE.target_namespace(), name)),
    })
});
static DECIMAL: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("decimal");
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: name.clone(),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), ANY_SIMPLE_TYPE.name()),
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
        variety: Some(Variety::Atomic(ANY_SIMPLE_TYPE.target_namespace(), name)),
    })
});
static FLOAT: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("float");
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: name.clone(),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), ANY_SIMPLE_TYPE.name()),
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
        variety: Some(Variety::Atomic(ANY_SIMPLE_TYPE.target_namespace(), name)),
    })
});
static DOUBLE: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("double");
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: name.clone(),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), ANY_SIMPLE_TYPE.name()),
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
        variety: Some(Variety::Atomic(ANY_SIMPLE_TYPE.target_namespace(), name)),
    })
});
static DURATION: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("duration");
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: name.clone(),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), ANY_SIMPLE_TYPE.name()),
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
        variety: Some(Variety::Atomic(ANY_SIMPLE_TYPE.target_namespace(), name)),
    })
});
static DATETIME: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("dateTime");
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: name.clone(),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), ANY_SIMPLE_TYPE.name()),
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
        variety: Some(Variety::Atomic(ANY_SIMPLE_TYPE.target_namespace(), name)),
    })
});
static TIME: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("time");
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: name.clone(),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), ANY_SIMPLE_TYPE.name()),
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
        variety: Some(Variety::Atomic(ANY_SIMPLE_TYPE.target_namespace(), name)),
    })
});
static DATE: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("date");
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: name.clone(),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), ANY_SIMPLE_TYPE.name()),
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
        variety: Some(Variety::Atomic(ANY_SIMPLE_TYPE.target_namespace(), name)),
    })
});
static GYEARMONTH: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("gYearMonth");
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: name.clone(),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), ANY_SIMPLE_TYPE.name()),
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
        variety: Some(Variety::Atomic(ANY_SIMPLE_TYPE.target_namespace(), name)),
    })
});
static GYEAR: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("gYear");
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: name.clone(),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), ANY_SIMPLE_TYPE.name()),
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
        variety: Some(Variety::Atomic(ANY_SIMPLE_TYPE.target_namespace(), name)),
    })
});
static GMONTHDAY: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("gMonthDay");
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: name.clone(),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), ANY_SIMPLE_TYPE.name()),
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
        variety: Some(Variety::Atomic(ANY_SIMPLE_TYPE.target_namespace(), name)),
    })
});
static GDAY: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("gDay");
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: name.clone(),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), ANY_SIMPLE_TYPE.name()),
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
        variety: Some(Variety::Atomic(ANY_SIMPLE_TYPE.target_namespace(), name)),
    })
});
static GMONTH: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("gMonth");
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: name.clone(),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), ANY_SIMPLE_TYPE.name()),
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
        variety: Some(Variety::Atomic(ANY_SIMPLE_TYPE.target_namespace(), name)),
    })
});
static HEXBINARY: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("hexBinary");
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: name.clone(),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), ANY_SIMPLE_TYPE.name()),
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
        variety: Some(Variety::Atomic(ANY_SIMPLE_TYPE.target_namespace(), name)),
    })
});
static BASE64BINARY: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("base64Binary");
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: name.clone(),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), ANY_SIMPLE_TYPE.name()),
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
        variety: Some(Variety::Atomic(ANY_SIMPLE_TYPE.target_namespace(), name)),
    })
});
static ANYURI: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("anyURI");
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: name.clone(),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), ANY_SIMPLE_TYPE.name()),
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
        variety: Some(Variety::Atomic(ANY_SIMPLE_TYPE.target_namespace(), name)),
    })
});
static QNAME: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("QName");
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: name.clone(),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), ANY_SIMPLE_TYPE.name()),
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
        variety: Some(Variety::Atomic(ANY_SIMPLE_TYPE.target_namespace(), name)),
    })
});
static NOTATION: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("NOTATION");
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: name.clone(),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), ANY_SIMPLE_TYPE.name()),
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
        variety: Some(Variety::Atomic(ANY_SIMPLE_TYPE.target_namespace(), name)),
    })
});
static NORMALIZED_STRING: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: Arc::from("normalizedString"),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), STRING.name()),
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
        variety: Some(Variety::Atomic(STRING.target_namespace(), STRING.name())),
    })
});
static TOKEN: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: Arc::from("token"),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), NORMALIZED_STRING.name()),
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
        variety: Some(Variety::Atomic(STRING.target_namespace(), STRING.name())),
    })
});
static LANGUAGE: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: Arc::from("language"),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), TOKEN.name()),
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
        variety: Some(Variety::Atomic(STRING.target_namespace(), STRING.name())),
    })
});
static NMTOKEN: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: Arc::from("NMTOKEN"),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), TOKEN.name()),
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
        variety: Some(Variety::Atomic(STRING.target_namespace(), STRING.name())),
    })
});
static NMTOKENS: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: Arc::from("NMTOKENS"),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), ANY_SIMPLE_TYPE.name()),
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
        variety: Some(Variety::List(TOKEN.target_namespace(), TOKEN.name())),
    })
});
static NAME: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: Arc::from("Name"),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), TOKEN.name()),
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
        variety: Some(Variety::Atomic(STRING.target_namespace(), STRING.name())),
    })
});
static NCNAME: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: Arc::from("NCName"),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), NAME.name()),
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
        variety: Some(Variety::Atomic(STRING.target_namespace(), STRING.name())),
    })
});
static ID: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: Arc::from("ID"),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), NCNAME.name()),
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
        variety: Some(Variety::Atomic(STRING.target_namespace(), STRING.name())),
    })
});
static IDREF: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: Arc::from("IDREF"),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), NCNAME.name()),
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
        variety: Some(Variety::Atomic(STRING.target_namespace(), STRING.name())),
    })
});
static IDREFS: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: Arc::from("IDREFS"),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), ANY_SIMPLE_TYPE.name()),
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
        variety: Some(Variety::List(IDREF.target_namespace(), IDREF.name())),
    })
});
static ENTITY: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: Arc::from("ENTITY"),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), NCNAME.name()),
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
        variety: Some(Variety::Atomic(STRING.target_namespace(), STRING.name())),
    })
});
static ENTITIES: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: Arc::from("ENTITIES"),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), ANY_SIMPLE_TYPE.name()),
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
        variety: Some(Variety::List(ENTITY.target_namespace(), ENTITY.name())),
    })
});
static INTEGER: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: Arc::from("integer"),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), DECIMAL.name()),
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
        variety: Some(Variety::Atomic(DECIMAL.target_namespace(), DECIMAL.name())),
    })
});
static NONPOSITIVE_INTEGER: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: Arc::from("nonPositiveInteger"),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), INTEGER.name()),
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
        variety: Some(Variety::Atomic(DECIMAL.target_namespace(), DECIMAL.name())),
    })
});
static NEGATIVE_INTEGER: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: Arc::from("negativeInteger"),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (
            ANY_SIMPLE_TYPE.target_namespace(),
            NONPOSITIVE_INTEGER.name(),
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
        variety: Some(Variety::Atomic(DECIMAL.target_namespace(), DECIMAL.name())),
    })
});
static LONG: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: Arc::from("long"),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), INTEGER.name()),
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
        variety: Some(Variety::Atomic(DECIMAL.target_namespace(), DECIMAL.name())),
    })
});
static INT: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: Arc::from("int"),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), LONG.name()),
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
        variety: Some(Variety::Atomic(DECIMAL.target_namespace(), DECIMAL.name())),
    })
});
static SHORT: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: Arc::from("short"),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), INT.name()),
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
        variety: Some(Variety::Atomic(DECIMAL.target_namespace(), DECIMAL.name())),
    })
});
static BYTE: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: Arc::from("byte"),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), SHORT.name()),
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
        variety: Some(Variety::Atomic(DECIMAL.target_namespace(), DECIMAL.name())),
    })
});
static NONNEGATIVE_INTEGER: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: Arc::from("nonNegativeInteger"),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), INTEGER.name()),
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
        variety: Some(Variety::Atomic(DECIMAL.target_namespace(), DECIMAL.name())),
    })
});
static UNSIGNED_LONG: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: Arc::from("unsignedLong"),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (
            ANY_SIMPLE_TYPE.target_namespace(),
            NONNEGATIVE_INTEGER.name(),
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
        variety: Some(Variety::Atomic(DECIMAL.target_namespace(), DECIMAL.name())),
    })
});
static UNSIGNED_INT: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: Arc::from("unsignedInt"),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), UNSIGNED_LONG.name()),
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
        variety: Some(Variety::Atomic(DECIMAL.target_namespace(), DECIMAL.name())),
    })
});
static UNSIGNED_SHORT: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: Arc::from("unsignedShort"),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), UNSIGNED_INT.name()),
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
        variety: Some(Variety::Atomic(DECIMAL.target_namespace(), DECIMAL.name())),
    })
});
static UNSIGNED_BYTE: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: Arc::from("unsignedByte"),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), UNSIGNED_SHORT.name()),
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
        variety: Some(Variety::Atomic(DECIMAL.target_namespace(), DECIMAL.name())),
    })
});
static POSITIVE_INTEGER: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(SimpleTypeDefinition {
        name: Arc::from("positiveInteger"),
        target_namespace: ANY_SIMPLE_TYPE.target_namespace(),
        base_type_definition: (
            ANY_SIMPLE_TYPE.target_namespace(),
            NONNEGATIVE_INTEGER.name(),
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
        variety: Some(Variety::Atomic(DECIMAL.target_namespace(), DECIMAL.name())),
    })
});

pub struct SimpleTypeDefinition {
    name: Arc<str>,
    target_namespace: Option<Arc<str>>,
    base_type_definition: (Option<Arc<str>>, Arc<str>),
    facets: Facets,
    fundamental_facets: FundamentalFacets,
    r#final: FinalSet,
    variety: Option<Variety>,
    // annotation: (),
}

impl SimpleTypeDefinition {
    pub fn restriction(
        target_namespace: Option<Arc<str>>,
        name: Arc<str>,
        base_type_definition: (Option<Arc<str>>, Arc<str>),
    ) -> Result<Self, XSDParseError> {
        if base_type_definition.0.as_deref() == Some(XML_SCHEMA_NAMESPACE)
            && base_type_definition.1.as_ref() == "anySimpleType"
        {
            // 3.14.1 (non-normative) The Simple Type Definition Schema Component
            // > The ·simple ur-type definition· must not be named as the ·base type
            // > definition· of any user-defined atomic simple type definitions: as
            // > it has no constraining facets, this would be incoherent.
            //
            // Although the description above exists, there is no corresponding SCC,
            // so for now, it will be treated as a “st-props-correct” error.
            return Err(XSDParseError::StPropsCorrect);
        }
        Ok(Self {
            name,
            target_namespace,
            base_type_definition,
            facets: Facets::default(),
            fundamental_facets: FundamentalFacets::default(),
            r#final: FinalSet::default(),
            // The base type definition's `variety` must be inherited,
            // but since its value is unknown at this point, `None` is set.
            variety: None,
        })
    }

    pub fn list(
        target_namespace: Option<Arc<str>>,
        name: Arc<str>,
        item_type: (Option<Arc<str>>, Arc<str>),
    ) -> Self {
        Self {
            name,
            target_namespace,
            base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), ANY_SIMPLE_TYPE.name()),
            facets: Facets::default(),
            fundamental_facets: FundamentalFacets::default(),
            r#final: FinalSet::default(),
            variety: Some(Variety::List(item_type.0, item_type.1)),
        }
    }

    pub fn union(
        target_namespace: Option<Arc<str>>,
        name: Arc<str>,
        member_types: Arc<[(Option<Arc<str>>, Arc<str>)]>,
    ) -> Self {
        Self {
            name,
            target_namespace,
            base_type_definition: (ANY_SIMPLE_TYPE.target_namespace(), ANY_SIMPLE_TYPE.name()),
            facets: Facets::default(),
            fundamental_facets: FundamentalFacets::default(),
            r#final: FinalSet::default(),
            variety: Some(Variety::Union(member_types)),
        }
    }

    pub fn length(&self) -> Option<usize> {
        self.facets.length.map(|l| l.value)
    }

    pub fn set_length(&mut self, length: usize, fixed: bool) -> Result<(), XSDParseError> {
        if self.facets.length.is_some() {
            return Err(XSDParseError::SrcSingleFacetValue);
        }
        match self.variety.as_ref() {
            Some(Variety::Union(_)) => return Err(XSDParseError::CosApplicableFacets),
            Some(Variety::Atomic(_, n))
                if !matches!(
                    n.as_ref(),
                    "string" | "hexBinary" | "base64Binary" | "anyURI" | "QName" | "NOTATION"
                ) =>
            {
                return Err(XSDParseError::CosApplicableFacets);
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

    pub fn set_min_length(&mut self, length: usize, fixed: bool) -> Result<(), XSDParseError> {
        if self.facets.min_length.is_some() {
            return Err(XSDParseError::SrcSingleFacetValue);
        }
        match self.variety.as_ref() {
            Some(Variety::Union(_)) => return Err(XSDParseError::CosApplicableFacets),
            Some(Variety::Atomic(_, n))
                if !matches!(
                    n.as_ref(),
                    "string" | "hexBinary" | "base64Binary" | "anyURI" | "QName" | "NOTATION"
                ) =>
            {
                return Err(XSDParseError::CosApplicableFacets);
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

    pub fn set_max_length(&mut self, length: usize, fixed: bool) -> Result<(), XSDParseError> {
        if self.facets.max_length.is_some() {
            return Err(XSDParseError::SrcSingleFacetValue);
        }
        match self.variety.as_ref() {
            Some(Variety::Union(_)) => return Err(XSDParseError::CosApplicableFacets),
            Some(Variety::Atomic(_, n))
                if !matches!(
                    n.as_ref(),
                    "string" | "hexBinary" | "base64Binary" | "anyURI" | "QName" | "NOTATION"
                ) =>
            {
                return Err(XSDParseError::CosApplicableFacets);
            }
            _ => {}
        }
        self.facets.max_length = Some(Length {
            value: length,
            fixed,
        });
        Ok(())
    }

    pub fn set_pattern(&mut self, pattern: impl Into<String>) -> Result<(), XSDParseError> {
        let pattern: String = pattern.into();
        if let Err(err) = XSRegexp::compile(&pattern) {
            return Err(XSDParseError::RegexpError(err));
        }
        self.facets.pattern.push(Pattern { value: pattern });
        Ok(())
    }

    pub fn set_enumeration(&mut self, value: impl Into<String>) -> Result<(), XSDParseError> {
        match self.variety.as_ref() {
            Some(Variety::Atomic(_, n)) if n.as_ref() == "boolean" => {
                return Err(XSDParseError::CosApplicableFacets);
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

    pub fn set_whitespace(&mut self, value: Whitespace, fixed: bool) -> Result<(), XSDParseError> {
        if self.facets.whitespace.is_some() {
            return Err(XSDParseError::SrcSingleFacetValue);
        }
        if matches!(self.variety, Some(Variety::Union(_))) {
            return Err(XSDParseError::CosApplicableFacets);
        }
        self.facets.whitespace = Some(WhitespaceConstraint { value, fixed });
        Ok(())
    }

    pub fn set_max_inclusive(
        &mut self,
        value: impl Into<String>,
        fixed: bool,
    ) -> Result<(), XSDParseError> {
        if self.facets.max_inclusive.is_some() {
            return Err(XSDParseError::SrcSingleFacetValue);
        }
        match self.variety.as_ref() {
            Some(Variety::List(_, _) | Variety::Union(_)) => {
                return Err(XSDParseError::CosApplicableFacets);
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
                return Err(XSDParseError::CosApplicableFacets);
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
    ) -> Result<(), XSDParseError> {
        if self.facets.max_exclusive.is_some() {
            return Err(XSDParseError::SrcSingleFacetValue);
        }
        match self.variety.as_ref() {
            Some(Variety::List(_, _) | Variety::Union(_)) => {
                return Err(XSDParseError::CosApplicableFacets);
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
                return Err(XSDParseError::CosApplicableFacets);
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
    ) -> Result<(), XSDParseError> {
        if self.facets.min_exclusive.is_some() {
            return Err(XSDParseError::SrcSingleFacetValue);
        }
        match self.variety.as_ref() {
            Some(Variety::List(_, _) | Variety::Union(_)) => {
                return Err(XSDParseError::CosApplicableFacets);
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
                return Err(XSDParseError::CosApplicableFacets);
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
    ) -> Result<(), XSDParseError> {
        if self.facets.min_inclusive.is_some() {
            return Err(XSDParseError::SrcSingleFacetValue);
        }
        match self.variety.as_ref() {
            Some(Variety::List(_, _) | Variety::Union(_)) => {
                return Err(XSDParseError::CosApplicableFacets);
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
                return Err(XSDParseError::CosApplicableFacets);
            }
            _ => {}
        }
        self.facets.min_inclusive = Some(Boundary {
            value: value.into(),
            fixed,
        });
        Ok(())
    }

    pub fn set_total_digits(
        &mut self,
        value: NonZeroUsize,
        fixed: bool,
    ) -> Result<(), XSDParseError> {
        if self.facets.total_digits.is_some() {
            return Err(XSDParseError::SrcSingleFacetValue);
        }
        match self.variety.as_ref() {
            Some(Variety::List(_, _) | Variety::Union(_)) => {
                return Err(XSDParseError::CosApplicableFacets);
            }
            Some(Variety::Atomic(_, n)) if !matches!(n.as_ref(), "decimal") => {
                return Err(XSDParseError::CosApplicableFacets);
            }
            _ => {}
        }
        self.facets.total_digits = Some(Length {
            value: value.get(),
            fixed,
        });
        Ok(())
    }

    pub fn set_fraction_digits(&mut self, value: usize, fixed: bool) -> Result<(), XSDParseError> {
        if self.facets.fraction_digits.is_some() {
            return Err(XSDParseError::SrcSingleFacetValue);
        }
        match self.variety.as_ref() {
            Some(Variety::List(_, _) | Variety::Union(_)) => {
                return Err(XSDParseError::CosApplicableFacets);
            }
            Some(Variety::Atomic(_, n)) if !matches!(n.as_ref(), "decimal") => {
                return Err(XSDParseError::CosApplicableFacets);
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
}

#[derive(Default)]
struct Facets {
    length: Option<Length>,
    min_length: Option<Length>,
    max_length: Option<Length>,
    pattern: Vec<Pattern>,
    enumeration: Vec<Enumeration>,
    whitespace: Option<WhitespaceConstraint>,
    max_inclusive: Option<Boundary>,
    max_exclusive: Option<Boundary>,
    min_exclusive: Option<Boundary>,
    min_inclusive: Option<Boundary>,
    total_digits: Option<Length>,
    fraction_digits: Option<Length>,
}

#[derive(Clone, Copy)]
struct Length {
    value: usize,
    fixed: bool,
    // annotation: (),
}

struct Pattern {
    value: String,
    // annotation: (),
}

struct Enumeration {
    value: String,
    // annotation: (),
}

#[derive(Clone, Copy)]
struct WhitespaceConstraint {
    value: Whitespace,
    fixed: bool,
    // annotation: (),
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

struct Boundary {
    value: String,
    fixed: bool,
    // annotation: (),
}

#[derive(Default)]
struct FundamentalFacets {
    ordered: Ordered,
    bounded: bool,
    cardinality: Cardinality,
    numeric: bool,
}

#[derive(Clone, Copy, Default)]
enum Ordered {
    #[default]
    False,
    Partial,
    Total,
}

#[derive(Clone, Copy, Default)]
enum Cardinality {
    #[default]
    CountablyInfinite,
    Finite,
}

#[derive(Clone)]
enum Variety {
    Atomic(Option<Arc<str>>, Arc<str>),
    List(Option<Arc<str>>, Arc<str>),
    Union(Arc<[(Option<Arc<str>>, Arc<str>)]>),
}

pub struct ComplexTypeDefinition {
    name: Arc<str>,
    target_namespace: Option<Arc<str>>,
    base_type_definition: (Option<Arc<str>>, Arc<str>),
    derivation_method: DerivationMethod,
    r#final: FinalSet,
    r#abstract: bool,
    attribute_uses: Vec<AttributeUse>,
    attribute_wildcard: Option<Wildcard>,
    content_type: ContentType,
    prohibited_substitutions: BlockSet,
    // annotation: (),
}

#[derive(Clone)]
enum ContentType {
    Empty,
    Simple(Option<Arc<str>>, Arc<str>),
    Mixed(Particle),
    ElementOnly(Particle),
}

pub enum TypeDefinition {
    Simple(SimpleTypeDefinition),
    Complex(ComplexTypeDefinition),
}

impl TypeDefinition {
    fn name(&self) -> Arc<str> {
        match self {
            TypeDefinition::Simple(def) => def.name.clone(),
            TypeDefinition::Complex(def) => def.name.clone(),
        }
    }

    fn target_namespace(&self) -> Option<Arc<str>> {
        match self {
            TypeDefinition::Simple(def) => def.target_namespace.clone(),
            TypeDefinition::Complex(def) => def.target_namespace.clone(),
        }
    }

    fn base_type_definition(&self) -> (Option<Arc<str>>, Arc<str>) {
        match self {
            TypeDefinition::Simple(def) => def.base_type_definition.clone(),
            TypeDefinition::Complex(def) => def.base_type_definition.clone(),
        }
    }

    pub fn is_simple_ur_type(&self) -> bool {
        match self {
            TypeDefinition::Simple(def) => {
                def.target_namespace.as_deref() == Some(XML_SCHEMA_NAMESPACE)
                    && def.name.as_ref() == "anySimpleType"
            }
            TypeDefinition::Complex(_) => false,
        }
    }

    pub fn is_ur_type(&self) -> bool {
        match self {
            TypeDefinition::Complex(def) => {
                def.target_namespace.as_deref() == Some(XML_SCHEMA_NAMESPACE)
                    && def.name.as_ref() == "anyType"
            }
            _ => false,
        }
    }

    pub fn as_simple(&self) -> Option<&SimpleTypeDefinition> {
        match self {
            TypeDefinition::Simple(def) => Some(def),
            _ => None,
        }
    }

    pub fn as_complex(&self) -> Option<&ComplexTypeDefinition> {
        match self {
            TypeDefinition::Complex(def) => Some(def),
            _ => None,
        }
    }
}

struct InternalSet(HashMap<(Option<Arc<str>>, Arc<str>), TypeDefinition>);

impl InternalSet {
    fn get(&self, key: &(Option<Arc<str>>, Arc<str>)) -> Option<&TypeDefinition> {
        if key.0.as_deref() == Some(XML_SCHEMA_NAMESPACE) {
            return match key.1.as_ref() {
                "string" => Some(&*STRING),
                "boolean" => Some(&*BOOLEAN),
                "decimal" => Some(&*DECIMAL),
                "float" => Some(&*FLOAT),
                "double" => Some(&*DOUBLE),
                "duration" => Some(&*DURATION),
                "dateTime" => Some(&*DATETIME),
                "time" => Some(&*TIME),
                "date" => Some(&*DATE),
                "gYearMonth" => Some(&*GYEARMONTH),
                "gYear" => Some(&*GYEAR),
                "gMonthDay" => Some(&*GMONTHDAY),
                "gDay" => Some(&*GDAY),
                "gMonth" => Some(&*GMONTH),
                "hexBinary" => Some(&*HEXBINARY),
                "base64Binary" => Some(&*BASE64BINARY),
                "anyURI" => Some(&*ANYURI),
                "QName" => Some(&*QNAME),
                "NOTATION" => Some(&*NOTATION),
                "normalizedString" => Some(&*NORMALIZED_STRING),
                "token" => Some(&*TOKEN),
                "language" => Some(&*LANGUAGE),
                "NMTOKEN" => Some(&*NMTOKEN),
                "NMTOKENS" => Some(&*NMTOKENS),
                "Name" => Some(&*NAME),
                "NCName" => Some(&*NCNAME),
                "ID" => Some(&*ID),
                "IDREF" => Some(&*IDREF),
                "IDREFS" => Some(&*IDREFS),
                "ENTITY" => Some(&*ENTITY),
                "ENTITIES" => Some(&*ENTITIES),
                "integer" => Some(&*INTEGER),
                "nonPositiveInteger" => Some(&*NONPOSITIVE_INTEGER),
                "negativeInteger" => Some(&*NEGATIVE_INTEGER),
                "long" => Some(&*LONG),
                "int" => Some(&*INT),
                "short" => Some(&*SHORT),
                "byte" => Some(&*BYTE),
                "nonNegativeInteger" => Some(&*NONNEGATIVE_INTEGER),
                "unsignedLong" => Some(&*UNSIGNED_LONG),
                "unsignedInt" => Some(&*UNSIGNED_INT),
                "unsignedShort" => Some(&*UNSIGNED_SHORT),
                "unsignedByte" => Some(&*UNSIGNED_BYTE),
                "positiveInteger" => Some(&*POSITIVE_INTEGER),
                "anySimpleType" => Some(&*ANY_SIMPLE_TYPE),
                _ => None,
            };
        }
        self.0.get(key)
    }
}

pub struct TypeSet {
    set: InternalSet,
    redefine_id: usize,
}

impl TypeSet {
    pub fn new() -> Self {
        Self {
            set: InternalSet(HashMap::new()),
            redefine_id: 0,
        }
    }

    pub fn derive(&mut self, mut typedef: TypeDefinition) -> Result<(), XSDParseError> {
        let base = typedef.base_type_definition();
        let is_circular = self.is_circular_definition(&typedef);
        let Some(base) = self.set.get(&base) else {
            // Although this is not strictly an error—since it should be handled in
            // accordance with the description in Chapter 5.3 of XSD Part 1—an error
            // is returned to help determine whether the base type could be resolved.
            return Err(XSDParseError::MissingSubComponent);
        };
        match typedef {
            TypeDefinition::Simple(ref mut def) => {
                if is_circular {
                    // [st-props-correct] clause 2
                    return Err(XSDParseError::StPropsCorrect);
                }
                let TypeDefinition::Simple(base) = base else {
                    // [st-props-correct] clause 1
                    return Err(XSDParseError::StPropsCorrect);
                };

                if def.variety.is_none() {
                    // `variety` is `None` only when it is derived by restriction.
                    // In this case, since `variety` must be inherited from the base
                    // type definition, it is filled in here.
                    def.variety = base.variety.clone();
                }

                if base.r#final.restriction() {
                    // [st-props-correct] clause 3
                    return Err(XSDParseError::StPropsCorrect);
                }

                // check min/max length
                if def
                    .facets
                    .min_length
                    .as_ref()
                    .zip(base.facets.min_length.as_ref())
                    .is_some_and(|(d, b)| d.value < b.value)
                {
                    // [minLength-valid-restriction]
                    return Err(XSDParseError::MinLengthValidRestriction);
                }
                def.facets.min_length = def.facets.min_length.or(base.facets.min_length);
                if def
                    .facets
                    .max_length
                    .as_ref()
                    .zip(base.facets.max_length.as_ref())
                    .is_some_and(|(d, b)| d.value > b.value)
                {
                    // [maxLength-valid-restriction]
                    return Err(XSDParseError::MaxLengthValidRestriction);
                }
                def.facets.max_length = def.facets.max_length.or(base.facets.max_length);
                if def
                    .facets
                    .min_length
                    .as_ref()
                    .zip(def.facets.max_length.as_ref())
                    .is_some_and(|(min, max)| min.value > max.value)
                {
                    // [minLength-less-than-equal-to-maxLength]
                    return Err(XSDParseError::MinLengthLessThanEqualToMaxLength);
                }

                // check length
                if def
                    .facets
                    .length
                    .as_ref()
                    .zip(base.facets.length.as_ref())
                    .is_some_and(|(d, b)| d.value != b.value)
                {
                    // [length-valid-restriction]
                    return Err(XSDParseError::LengthValidRestriction);
                }
                def.facets.length = def.facets.length.or(base.facets.length);
                if def.facets.length.as_ref().is_some_and(|l| {
                    def.facets
                        .min_length
                        .as_ref()
                        .is_some_and(|min| min.value > l.value)
                        || def
                            .facets
                            .max_length
                            .as_ref()
                            .is_some_and(|max| max.value < l.value)
                }) {
                    // [length-minLength-maxLength]
                    return Err(XSDParseError::LengthMinLengthMaxLength);
                }

                // check whitespace
                if def
                    .facets
                    .whitespace
                    .as_ref()
                    .zip(base.facets.whitespace.as_ref())
                    .is_some_and(|(d, b)| d.value < b.value)
                {
                    // [whiteSpace-valid-restriction]
                    return Err(XSDParseError::WhiteSpaceValidRestriction);
                }
                def.facets.whitespace = def.facets.whitespace.or(base.facets.whitespace);

                // check total digits
                if def
                    .facets
                    .total_digits
                    .as_ref()
                    .zip(base.facets.total_digits.as_ref())
                    .is_some_and(|(d, b)| d.value > b.value)
                {
                    // [totalDigits-valid-restriction]
                    return Err(XSDParseError::TotalDigitsValidRestriction);
                }
                def.facets.total_digits = def.facets.total_digits.or(base.facets.total_digits);

                // check fraction digits
                if def
                    .facets
                    .fraction_digits
                    .as_ref()
                    .zip(base.facets.fraction_digits.as_ref())
                    .is_some_and(|(d, b)| d.value > b.value)
                {
                    // [fractionDigits-valid-restriction]
                    return Err(XSDParseError::FractionDigitsValidRestriction);
                }
                def.facets.fraction_digits =
                    def.facets.fraction_digits.or(base.facets.fraction_digits);
                if def
                    .facets
                    .fraction_digits
                    .as_ref()
                    .zip(def.facets.total_digits.as_ref())
                    .is_some_and(|(f, t)| f.value > t.value)
                {
                    // [fractionDigits-totalDigits]
                    return Err(XSDParseError::FractionDigitsTotalDigits);
                }

                match def.variety.as_ref() {
                    Some(Variety::Atomic(_, n)) => {
                        if !matches!(base.variety, Some(Variety::Atomic(_, _))) {
                            // [cos-st-restricts] clause 1.1
                            return Err(XSDParseError::CosStRestricts);
                        }
                        def.fundamental_facets.ordered = base.fundamental_facets.ordered;
                        def.fundamental_facets.bounded = (def.facets.min_exclusive.is_some()
                            || def.facets.min_inclusive.is_some())
                            && (def.facets.max_exclusive.is_some()
                                || def.facets.max_inclusive.is_some());
                        def.fundamental_facets.cardinality =
                            if matches!(base.fundamental_facets.cardinality, Cardinality::Finite)
                                || (def.length().is_some()
                                    || def.max_length().is_some()
                                    || def.facets.total_digits.is_some())
                            {
                                Cardinality::Finite
                            } else if (def.facets.min_inclusive.is_none()
                                && def.facets.min_exclusive.is_none())
                                || (def.facets.max_inclusive.is_none()
                                    && def.facets.max_exclusive.is_none())
                            {
                                Cardinality::CountablyInfinite
                            } else if def.facets.fraction_digits.is_some()
                                || matches!(
                                    n.as_ref(),
                                    "data"
                                        | "gYearMonth"
                                        | "gYear"
                                        | "gMonthDay"
                                        | "gDay"
                                        | "gMonth"
                                )
                            {
                                Cardinality::Finite
                            } else {
                                Cardinality::CountablyInfinite
                            };
                        def.fundamental_facets.numeric = base.fundamental_facets.numeric;
                    }
                    Some(Variety::List(ns, local)) => {
                        let Some(TypeDefinition::Simple(itemdef)) =
                            self.set.get(&(ns.clone(), local.clone()))
                        else {
                            return Err(XSDParseError::MissingSubComponent);
                        };
                        match itemdef.variety.as_ref() {
                            Some(Variety::Atomic(_, _)) => {}
                            Some(Variety::Union(members)) => {
                                for member in members.iter() {
                                    let Some(TypeDefinition::Simple(def)) = self.set.get(member)
                                    else {
                                        return Err(XSDParseError::MissingSubComponent);
                                    };
                                    if !matches!(def.variety, Some(Variety::Atomic(_, _))) {
                                        // [st-props-correct] clause 2.1
                                        return Err(XSDParseError::StPropsCorrect);
                                    }
                                }
                            }
                            _ => {
                                // [cos-list-of-atomic] and [st-props-correct] clause 2.1
                                return Err(XSDParseError::CosListOfAtomic);
                            }
                        }

                        // [cos-st-restricts] clause 2.3
                        if base.is_simple_ur_type() {
                            // [cos-st-restricts] clause 2.3.1
                            if itemdef.r#final.list() {
                                // [cos-st-restricts] clause 2.3.1.1
                                return Err(XSDParseError::CosStRestricts);
                            }
                            if def.facets.length.is_some()
                                || def.facets.min_length.is_some()
                                || def.facets.max_length.is_some()
                                || !def.facets.pattern.is_empty()
                                || !def.facets.enumeration.is_empty()
                                || def.facets.max_inclusive.is_some()
                                || def.facets.max_exclusive.is_some()
                                || def.facets.min_exclusive.is_some()
                                || def.facets.min_inclusive.is_some()
                                || def.facets.total_digits.is_some()
                                || def.facets.fraction_digits.is_some()
                            {
                                // [cos-st-restricts] clause 2.3.1.2
                                return Err(XSDParseError::CosStRestricts);
                            }
                        } else {
                            // [cos-st-restricts] clause 2.3.2
                            if !matches!(base.variety, Some(Variety::List(_, _))) {
                                // [cos-st-restricts] clause 2.3.2.1
                                return Err(XSDParseError::CosStRestricts);
                            }
                            if base.r#final.restriction() {
                                // [cos-st-restricts] clause 2.3.2.2
                                return Err(XSDParseError::CosStRestricts);
                            }
                            if def.facets.max_inclusive.is_some()
                                || def.facets.max_exclusive.is_some()
                                || def.facets.min_exclusive.is_some()
                                || def.facets.min_inclusive.is_some()
                                || def.facets.total_digits.is_some()
                                || def.facets.fraction_digits.is_some()
                            {
                                // [cos-st-restricts] clause 2.3.2.4
                                return Err(XSDParseError::CosStRestricts);
                            }
                        }

                        def.fundamental_facets.ordered = Ordered::False;
                        if def.length().is_some()
                            || (def.min_length().is_some() && def.max_length().is_some())
                        {
                            def.fundamental_facets.bounded = true;
                            def.fundamental_facets.cardinality = Cardinality::Finite;
                        } else {
                            def.fundamental_facets.bounded = false;
                            def.fundamental_facets.cardinality = Cardinality::CountablyInfinite;
                        }
                        def.fundamental_facets.numeric = false;
                    }
                    Some(Variety::Union(members)) => {
                        if members.is_empty() {
                            return Err(XSDParseError::SrcUnionMemberTypesOrSimpleTypes);
                        }

                        let members = members
                            .iter()
                            .map(|m| self.set.get(m))
                            .map(|t| t.and_then(|t| t.as_simple()))
                            .map(|t| t.ok_or(XSDParseError::MissingSubComponent))
                            .collect::<Result<Vec<_>, XSDParseError>>()?;

                        // Skip verification of [cos-st-restricts] clause 3.1.
                        // By the definition of a component, the union contained in
                        // memberTypes is expanded into its own memberTypes, so it
                        // cannot contain a union. Since expanding it would be a waste
                        // of memory, leave it as is with the union included.

                        if base.is_simple_ur_type() {
                            // [cos-st-restricts] clause 3.3.1
                            if members.iter().any(|m| m.r#final.union()) {
                                // [cos-st-restricts] clause 3.3.1.1
                                return Err(XSDParseError::CosStRestricts);
                            }
                            if def.facets.length.is_some()
                                || def.facets.min_length.is_some()
                                || def.facets.max_length.is_some()
                                || !def.facets.pattern.is_empty()
                                || !def.facets.enumeration.is_empty()
                                || def.facets.whitespace.is_some()
                                || def.facets.max_inclusive.is_some()
                                || def.facets.max_exclusive.is_some()
                                || def.facets.min_exclusive.is_some()
                                || def.facets.min_inclusive.is_some()
                                || def.facets.total_digits.is_some()
                                || def.facets.fraction_digits.is_some()
                            {
                                // [cos-st-restricts] clause 3.3.1.2
                                return Err(XSDParseError::CosStRestricts);
                            }
                        } else {
                            // [cos-st-restricts] clause 3.3.2
                            if !matches!(base.variety, Some(Variety::Union(_))) {
                                // [cos-st-restricts] clause 3.3.2.1
                                return Err(XSDParseError::CosStRestricts);
                            }
                            if base.r#final.restriction() {
                                // [cos-st-restricts] clause 3.3.2.2
                                return Err(XSDParseError::CosStRestricts);
                            }
                            if def.facets.length.is_some()
                                || def.facets.min_length.is_some()
                                || def.facets.max_length.is_some()
                                || def.facets.whitespace.is_some()
                                || def.facets.max_inclusive.is_some()
                                || def.facets.max_exclusive.is_some()
                                || def.facets.min_exclusive.is_some()
                                || def.facets.min_inclusive.is_some()
                                || def.facets.total_digits.is_some()
                                || def.facets.fraction_digits.is_some()
                            {
                                // [cos-st-restricts] clause 3.3.2.4
                                return Err(XSDParseError::CosStRestricts);
                            }
                        }
                    }
                    None => {
                        // This pattern should not occur, but if it does,
                        // configure the expected facets for `anySimpleType`.
                        def.fundamental_facets = Default::default();
                    }
                }
            }
            TypeDefinition::Complex(_) => {}
        }

        let key = (typedef.target_namespace(), typedef.name());
        self.set.0.insert(key, typedef);
        Ok(())
    }

    pub fn redefine(&mut self, mut typedef: TypeDefinition) -> Result<(), XSDParseError> {
        let key = (typedef.target_namespace(), typedef.name());
        let Some(base) = self.set.0.remove(&key) else {
            return Err(XSDParseError::MissingSubComponent);
        };

        let id = self.redefine_id;
        self.redefine_id += 1;

        let new_key: (Option<Arc<str>>, Arc<str>) =
            (key.0.clone(), format!("redefine{id}#{}", key.1).into());
        self.set.0.insert(new_key.clone(), base);
        match &mut typedef {
            TypeDefinition::Simple(def) => def.base_type_definition = new_key.clone(),
            TypeDefinition::Complex(def) => def.base_type_definition = new_key.clone(),
        }

        if let Err(err) = self.derive(typedef) {
            if let Some(base) = self.set.0.remove(&new_key) {
                self.set.0.insert(key, base);
            }
            return Err(err);
        }
        Ok(())
    }

    /// Returns `true` if not reaching the ur-type definition by tracing
    /// the base type definition of `def`; otherwise, returns `false`.
    fn is_circular_definition(&self, def: &TypeDefinition) -> bool {
        let mut memo = HashSet::new();
        memo.insert((def.target_namespace(), def.name()));
        let mut base = def.base_type_definition();
        while let Some(def) = self.set.get(&base) {
            if def.is_ur_type() {
                break;
            }
            if memo.contains(&(def.target_namespace(), def.name())) {
                return true;
            }
            memo.insert((def.target_namespace(), def.name()));
            base = def.base_type_definition();
        }
        false
    }
}

#[derive(Clone, Copy)]
enum DerivationMethod {
    Extension,
    Restriction,
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

#[derive(Clone, Copy, Default)]
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

#[derive(Clone, Copy)]
pub struct BlockSet(u8);

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
