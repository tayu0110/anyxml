use std::{
    num::NonZeroUsize,
    sync::{Arc, LazyLock},
};

use crate::{
    automata::xsregexp::XSRegexp,
    xsd::{XML_SCHEMA_NAMESPACE, XSDError, parse::TypeDefinition},
};

static ANY_SIMPLE_TYPE: LazyLock<TypeDefinition> = LazyLock::new(|| {
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
static STRING: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("string");
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static BOOLEAN: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("boolean");
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static DECIMAL: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("decimal");
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static FLOAT: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("float");
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static DOUBLE: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("double");
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static DURATION: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("duration");
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static DATETIME: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("dateTime");
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static TIME: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("time");
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static DATE: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("date");
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static GYEARMONTH: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("gYearMonth");
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static GYEAR: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("gYear");
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static GMONTHDAY: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("gMonthDay");
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static GDAY: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("gDay");
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static GMONTH: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("gMonth");
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static HEXBINARY: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("hexBinary");
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static BASE64BINARY: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("base64Binary");
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static ANYURI: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("anyURI");
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static QNAME: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("QName");
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static NOTATION: LazyLock<TypeDefinition> = LazyLock::new(|| {
    let name: Arc<str> = Arc::from("NOTATION");
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static NORMALIZED_STRING: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static TOKEN: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static LANGUAGE: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static NMTOKEN: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static NMTOKENS: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static NAME: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static NCNAME: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static ID: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static IDREF: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static IDREFS: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static ENTITY: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static ENTITIES: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static INTEGER: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static NONPOSITIVE_INTEGER: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static NEGATIVE_INTEGER: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static LONG: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static INT: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static SHORT: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static BYTE: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static NONNEGATIVE_INTEGER: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static UNSIGNED_LONG: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static UNSIGNED_INT: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static UNSIGNED_SHORT: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static UNSIGNED_BYTE: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});
static POSITIVE_INTEGER: LazyLock<TypeDefinition> = LazyLock::new(|| {
    TypeDefinition::Simple(Arc::new(SimpleTypeDefinition {
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
    }))
});

#[derive(Default)]
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
}

#[derive(Default)]
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

pub(super) struct Pattern {
    pub(super) value: String,
}

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

pub(super) struct Boundary {
    pub(super) value: String,
    pub(super) fixed: bool,
}

#[derive(Default)]
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

#[derive(Clone)]
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
