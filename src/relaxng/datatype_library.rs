use std::{
    collections::{BTreeMap, HashMap},
    sync::Arc,
};

use crate::{XMLVersion, uri::URIStr};

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
        params: &BTreeMap<Arc<str>, Arc<str>>,
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
        params: &BTreeMap<Arc<str>, Arc<str>>,
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
        _params: &BTreeMap<Arc<str>, Arc<str>>,
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
        params: &BTreeMap<Arc<str>, Arc<str>>,
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
        libraries
    }
}
