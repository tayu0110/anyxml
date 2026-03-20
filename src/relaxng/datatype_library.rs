use std::{
    collections::{BTreeMap, HashMap},
    num::NonZeroUsize,
    sync::Arc,
};

use crate::{
    XMLVersion,
    sax::NamespaceStack,
    uri::URIStr,
    xsdtypes::{
        FacetType, SimpleTypeDefinition, XML_SCHEMA_DATATYPES_NAMESPACE,
        find_builtin_type_definition,
    },
};

pub trait RelaxNGDatatypeLibrary: Send + Sync {
    /// If a type named `type_name` exists in the library, return `true`,
    /// otherwise return `false`.
    fn contains(&self, type_name: &str) -> bool;
    /// If a type named `type_name` exists in the library, return [`Some`] wrapped around
    /// [`true`] if `value` is a valid representation of that type, or [`false`] otherwise.
    ///
    /// Even if a type exists, if the arguments sufficient for determination are not included
    /// in `params`, [`None`] is returned.
    ///
    /// If a type named `type_name` does not exist in the library, return [`None`].
    ///
    /// # Reference
    /// ISO/IEC 19757-2:2008 9.3.8 data and value pattern
    fn validate(
        &self,
        type_name: &str,
        params: &[(Arc<str>, Arc<str>)],
        value: &str,
        context: &(Arc<URIStr>, BTreeMap<Arc<str>, Arc<str>>),
    ) -> Option<bool>;
    /// If a type named `type_name` exists in the library, return [`Some`] wrapped around
    /// [`true`] if `params` is a valid parameter list of that type, or [`false`] otherwise.
    ///
    /// If a type named `type_name` does not exist in the library, return [`None`].
    fn validate_params(
        &self,
        type_name: &str,
        params: &[(Arc<str>, Arc<str>)],
        context: &(Arc<URIStr>, BTreeMap<Arc<str>, Arc<str>>),
    ) -> Option<bool>;
    /// If a type named `type_name` exists in the library, return [`Some`] wrapped around
    /// [`true`] if `lhs` and `rhs` are equal as representations of that type,
    /// or [`false`] if they are not equal.
    ///
    /// If a type named `type_name` does not exist in the library, returns [`None`].
    ///
    /// # Reference
    /// ISO/IEC 19757-2:2008 9.3.8 data and value pattern
    fn eq(
        &self,
        type_name: &str,
        lhs: &str,
        cx1: &(Arc<URIStr>, BTreeMap<Arc<str>, Arc<str>>),
        rhs: &str,
        cx2: &(Arc<URIStr>, BTreeMap<Arc<str>, Arc<str>>),
    ) -> Option<bool>;
}

pub struct RelaxNGBuiltinDatatypeLibrary;

impl RelaxNGDatatypeLibrary for RelaxNGBuiltinDatatypeLibrary {
    fn contains(&self, type_name: &str) -> bool {
        matches!(type_name, "string" | "token")
    }

    fn validate(
        &self,
        type_name: &str,
        _params: &[(Arc<str>, Arc<str>)],
        _value: &str,
        _context: &(Arc<URIStr>, BTreeMap<Arc<str>, Arc<str>>),
    ) -> Option<bool> {
        match type_name {
            "string" | "token" => Some(true),
            _ => None,
        }
    }

    fn validate_params(
        &self,
        type_name: &str,
        params: &[(Arc<str>, Arc<str>)],
        _context: &(Arc<URIStr>, BTreeMap<Arc<str>, Arc<str>>),
    ) -> Option<bool> {
        self.contains(type_name).then_some(params.is_empty())
    }

    fn eq(
        &self,
        type_name: &str,
        lhs: &str,
        _cx1: &(Arc<URIStr>, BTreeMap<Arc<str>, Arc<str>>),
        rhs: &str,
        _cx2: &(Arc<URIStr>, BTreeMap<Arc<str>, Arc<str>>),
    ) -> Option<bool> {
        match type_name {
            "string" => Some(lhs == rhs),
            "token" => {
                let lhs = lhs
                    .split(|c: char| XMLVersion::default().is_whitespace(c))
                    .filter(|s| !s.is_empty())
                    .collect::<Vec<_>>();
                let rhs = rhs
                    .split(|c: char| XMLVersion::default().is_whitespace(c))
                    .filter(|s| !s.is_empty())
                    .collect::<Vec<_>>();

                Some(lhs == rhs)
            }
            _ => None,
        }
    }
}

/// # Reference
/// - [Guidelines for using W3C XML Schema Datatypes with RELAX NG](https://relaxng.org/xsd-20010907.html)
pub struct XSDTypeLibrary;

impl XSDTypeLibrary {
    fn get(
        &self,
        type_name: &str,
        params: &[(Arc<str>, Arc<str>)],
        namespaces: &NamespaceStack,
    ) -> Option<Arc<SimpleTypeDefinition>> {
        let typedef = find_builtin_type_definition(type_name)?;
        let mut builder = typedef.clone().derive_by_restriction().ok()?;
        for (k, v) in params {
            builder = match k.parse::<FacetType>().ok()? {
                FacetType::Length => {
                    let length = v.parse::<usize>().ok()?;
                    builder.length(length).ok()?
                }
                FacetType::MinLength => {
                    let length = v.parse::<usize>().ok()?;
                    builder.min_length(length).ok()?
                }
                FacetType::MaxLength => {
                    let length = v.parse::<usize>().ok()?;
                    builder.max_length(length).ok()?
                }
                FacetType::Pattern => {
                    // Since processing `pattern` increases the number of derivation steps,
                    // it must be performed last.
                    //
                    // As indicated by [SCC: length and minLength or maxLength], there are
                    // sets of facets that cannot be specified at the same time in different
                    // derivation steps; therefore, facets other than `pattern` must be
                    // treated as if they were specified in a single derivation step.
                    builder
                }
                FacetType::Enumeration | FacetType::WhiteSpace => {
                    // 2. Parameters
                    // - whiteSpace (the builtin derived datatype that specifies the
                    //   desired value for the whiteSpace facet should be used instead)
                    // - enumeration (the value element should be used instead)
                    return None;
                }
                FacetType::MaxInclusive => {
                    let value = typedef.parse(v.as_ref(), namespaces).ok()?;
                    builder.max_inclusive(value).ok()?
                }
                FacetType::MaxExclusive => {
                    let value = typedef.parse(v.as_ref(), namespaces).ok()?;
                    builder.max_exclusive(value).ok()?
                }
                FacetType::MinExclusive => {
                    let value = typedef.parse(v.as_ref(), namespaces).ok()?;
                    builder.min_exclusive(value).ok()?
                }
                FacetType::MinInclusive => {
                    let value = typedef.parse(v.as_ref(), namespaces).ok()?;
                    builder.min_inclusive(value).ok()?
                }
                FacetType::TotalDigits => {
                    let digits = v.parse().ok().and_then(NonZeroUsize::new)?;
                    builder.total_digits(digits).ok()?
                }
                FacetType::FractionDigits => {
                    let digits = v.parse::<usize>().ok()?;
                    builder.fraction_digits(digits).ok()?
                }
            };
        }

        for (k, v) in params {
            if let Ok(FacetType::Pattern) = k.parse() {
                // Unlike the XML representation of XSD type definitions, since multiple
                // `pattern`s are combined with an AND condition, each `pattern` must be
                // treated as if it were specified in a separate derivation step.
                //
                // 2. Parameters
                // If the pattern parameter is specified more than once for a single data element,
                // then a string matches the data element only if it matches all of the patterns.
                builder = builder
                    .pattern(v.as_ref())
                    .ok()?
                    .build()
                    .derive_by_restriction()
                    .ok()?;
            }
        }
        Some(builder.build())
    }
}

impl RelaxNGDatatypeLibrary for XSDTypeLibrary {
    fn contains(&self, type_name: &str) -> bool {
        find_builtin_type_definition(type_name).is_some()
    }

    fn validate(
        &self,
        type_name: &str,
        params: &[(Arc<str>, Arc<str>)],
        value: &str,
        context: &(Arc<URIStr>, BTreeMap<Arc<str>, Arc<str>>),
    ) -> Option<bool> {
        let mut namespaces = NamespaceStack::default();
        for (k, v) in &context.1 {
            namespaces.push(k.as_ref(), v.as_ref());
        }
        let t = self.get(type_name, params, &namespaces)?;
        Some(t.parse(value, &namespaces).is_ok())
    }

    fn validate_params(
        &self,
        type_name: &str,
        params: &[(Arc<str>, Arc<str>)],
        context: &(Arc<URIStr>, BTreeMap<Arc<str>, Arc<str>>),
    ) -> Option<bool> {
        // This line is necessary because `None` is returned only
        // when there is no type corresponding to the type name.
        find_builtin_type_definition(type_name)?;
        let mut namespaces = NamespaceStack::default();
        for (k, v) in &context.1 {
            namespaces.push(k.as_ref(), v.as_ref());
        }
        Some(self.get(type_name, params, &namespaces).is_some())
    }

    fn eq(
        &self,
        type_name: &str,
        lhs: &str,
        cx1: &(Arc<URIStr>, BTreeMap<Arc<str>, Arc<str>>),
        rhs: &str,
        cx2: &(Arc<URIStr>, BTreeMap<Arc<str>, Arc<str>>),
    ) -> Option<bool> {
        let typedef = find_builtin_type_definition(type_name)?;

        let mut ns1 = NamespaceStack::default();
        for (k, v) in &cx1.1 {
            ns1.push(k.as_ref(), v.as_ref());
        }
        let mut ns2 = NamespaceStack::default();
        for (k, v) in &cx2.1 {
            ns2.push(k.as_ref(), v.as_ref());
        }
        Some(
            typedef
                .parse(lhs, &ns1)
                .ok()
                .zip(typedef.parse(rhs, &ns2).ok())
                .is_some_and(|(lh, rh)| lh == rh),
        )
    }
}

#[derive(Clone)]
pub struct RelaxNGDatatypeLibraries {
    map: HashMap<Arc<str>, Arc<dyn RelaxNGDatatypeLibrary>>,
}

impl RelaxNGDatatypeLibraries {
    pub(super) fn get(&self, namespace_name: &str) -> Option<&dyn RelaxNGDatatypeLibrary> {
        self.map.get(namespace_name).map(|library| &**library)
    }

    fn insert(
        &mut self,
        namespace_name: Arc<str>,
        library: Arc<dyn RelaxNGDatatypeLibrary>,
    ) -> Option<Arc<dyn RelaxNGDatatypeLibrary>> {
        self.map.insert(namespace_name, library)
    }
}

impl Default for RelaxNGDatatypeLibraries {
    fn default() -> Self {
        let mut libraries = Self {
            map: HashMap::new(),
        };
        libraries.insert("".into(), Arc::new(RelaxNGBuiltinDatatypeLibrary));
        libraries.insert(
            XML_SCHEMA_DATATYPES_NAMESPACE.into(),
            Arc::new(XSDTypeLibrary),
        );
        libraries
    }
}
