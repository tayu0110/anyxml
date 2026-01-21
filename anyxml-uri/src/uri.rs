use std::{
    borrow::{Borrow, Cow},
    ops::Deref,
    path::Path,
    rc::Rc,
    str::{from_utf8, from_utf8_unchecked},
    sync::Arc,
};

use crate::ParseRIError;

#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct URIStr {
    uri: str,
}

impl URIStr {
    fn new(s: &str) -> &Self {
        unsafe {
            // # Safety
            // Since `URIStr` is a transparent newtype of `str`,
            // the bit patterns are exactly the same and have the same features.
            &*(s as *const str as *const Self)
        }
    }

    /// Resolve the relative reference `reference` using `self` as the base URI.
    ///
    /// `self` must be convertible to an [absolute URI](https://datatracker.ietf.org/doc/html/rfc3986#section-4.3)
    /// through [fragment](https://datatracker.ietf.org/doc/html/rfc3986#section-3.5) removal
    /// and normalization.
    ///
    /// # Reference
    /// - [5.1.  Establishing a Base URI](https://datatracker.ietf.org/doc/html/rfc3986#section-5.1)
    /// - [5.2.  Relative Resolution](https://datatracker.ietf.org/doc/html/rfc3986#section-5.2)
    pub fn resolve(&self, reference: &Self) -> URIString {
        use Component::*;

        let base = if self.is_absolute() {
            Cow::Borrowed(self)
        } else {
            let mut base = self.to_owned();
            base.normalize();
            if let Some(frag) = base.uri.bytes().position(|b| b == b'#') {
                base.uri.truncate(frag);
            }
            assert!(
                base.is_absolute(),
                "'{}' is not absolute",
                base.as_escaped_str()
            );
            Cow::Owned(base)
        };

        let mut ref_components = reference.components().peekable();
        if ref_components
            .next_if(|comp| matches!(comp, Scheme(_)))
            .is_some()
        {
            let mut ret = reference.to_owned();
            ret.normalize();
            return ret;
        }

        if ref_components
            .next_if(|comp| matches!(comp, Authority { .. }))
            .is_some()
        {
            // has authority
            let mut ret = URIString {
                uri: [base.scheme().unwrap(), ":", &reference.uri].concat(),
            };
            ret.normalize();
            return ret;
        }

        let mut components = base.components().peekable();
        let mut uri = String::new();
        if let Some(Scheme(scheme)) = components.next_if(|comp| matches!(comp, Scheme(_))) {
            uri.push_str(scheme);
            uri.push(':');
        }
        if let Some(Authority {
            userinfo,
            host,
            port,
        }) = components.next_if(|comp| matches!(comp, Authority { .. }))
        {
            uri.push_str("//");
            if let Some(userinfo) = userinfo {
                uri.push_str(userinfo);
                uri.push(':');
            }
            uri.push_str(host);
            if let Some(port) = port {
                uri.push(':');
                uri.push_str(port);
            }
        }

        if ref_components
            .next_if(|comp| matches!(comp, RootSegment))
            .is_some()
        {
            uri.push_str(&reference.uri);
            let mut ret = URIString { uri };
            ret.normalize();
            return ret;
        }

        let mut segments = vec![];
        let has_root = components
            .next_if(|comp| matches!(comp, RootSegment))
            .is_some();
        let mut has_dot_segment = false;
        while let Some(Segment(segment)) = components.next_if(|comp| matches!(comp, Segment(_))) {
            segments.push(segment);
            has_dot_segment |= segment == "." || segment == "..";
        }
        if has_dot_segment {
            segments = normalize_path_segments(segments.into_iter(), has_root);
        }

        let mut has_path = false;
        if let Some(Segment(segment)) = ref_components.next_if(|comp| matches!(comp, Segment(_))) {
            let mut buf = vec![segment];
            while let Some(Segment(segment)) =
                ref_components.next_if(|comp| matches!(comp, Segment(_)))
            {
                buf.push(segment);
            }
            if buf.len() > 1 || !buf[0].is_empty() {
                segments.pop();
                segments.extend(buf);
                has_path = true;
            }
        }
        build_normalized_path(segments.into_iter(), has_root, &mut uri);

        if let Some(Query(query)) = ref_components.next_if(|comp| matches!(comp, Query(_))) {
            uri.push('?');
            uri.push_str(query);
        } else if !has_path
            && let Some(Query(query)) = components.next_if(|comp| matches!(comp, Query(_)))
        {
            uri.push('?');
            uri.push_str(query);
        }

        if let Some(Fragment(fragment)) = ref_components.next() {
            uri.push('#');
            uri.push_str(fragment);
        }

        URIString { uri }
    }

    /// Return the escaped URI string.
    pub fn as_escaped_str(&self) -> &str {
        &self.uri
    }

    /// Return the unescaped URI string.  \
    /// If unescaping fails, return `None`.
    pub fn as_unescaped_str(&self) -> Option<Cow<'_, str>> {
        unescape(&self.uri).ok()
    }

    /// # Reference
    /// [4.3.  Absolute URI](https://datatracker.ietf.org/doc/html/rfc3986#section-4.3)
    pub fn is_absolute(&self) -> bool {
        self.scheme().is_some() && self.fragment().is_none()
    }

    /// # Reference
    /// [4.2.  Relative Reference](https://datatracker.ietf.org/doc/html/rfc3986#section-4.2)
    pub fn is_relative(&self) -> bool {
        self.scheme().is_none()
    }

    /// # Reference
    /// [3.1.  Scheme](https://datatracker.ietf.org/doc/html/rfc3986#section-3.1)
    pub fn scheme(&self) -> Option<&str> {
        let pos = self.uri.bytes().position(is_reserved)?;
        (self.uri.as_bytes()[pos] == b':').then_some(&self.uri[..pos])
    }

    /// # Reference
    /// [3.2.  Authority](https://datatracker.ietf.org/doc/html/rfc3986#section-3.2)
    pub fn authority(&self) -> Option<&str> {
        let rem = self
            .uri
            .strip_prefix("//")
            .or_else(|| self.uri.split_once("://").map(|p| p.1))?;
        Some(rem.split_once('/').map(|p| p.0).unwrap_or(rem))
    }

    /// # Reference
    /// [3.2.1.  User Information](https://datatracker.ietf.org/doc/html/rfc3986#section-3.2.1)
    pub fn userinfo(&self) -> Option<&str> {
        Some(self.authority()?.split_once('@')?.0)
    }

    /// # Reference
    /// [3.2.2.  Host](https://datatracker.ietf.org/doc/html/rfc3986#section-3.2.2)
    pub fn host(&self) -> Option<&str> {
        let mut auth = self.authority()?;
        if let Some((_userinfo, rem)) = auth.split_once('@') {
            auth = rem;
        }
        if let Some((host, port)) = auth.rsplit_once(':')
            && port.bytes().all(|b| b.is_ascii_digit())
        {
            auth = host;
        }
        Some(auth)
    }

    /// # Reference
    /// [3.2.3.  Port](https://datatracker.ietf.org/doc/html/rfc3986#section-3.2.3)
    pub fn port(&self) -> Option<&str> {
        let (_, port) = self.authority()?.rsplit_once(':')?;
        port.bytes().all(|b| b.is_ascii_digit()).then_some(port)
    }

    /// # Reference
    /// [3.3.  Path](https://datatracker.ietf.org/doc/html/rfc3986#section-3.3)
    pub fn path(&self) -> &str {
        let mut path = &self.uri;
        if let Some(scheme) = self.scheme() {
            // has scheme
            path = &path[scheme.len() + 1..];
        }
        if let Some(rem) = path.strip_prefix("//") {
            // has authority
            let pos = rem.bytes().position(|b| b == b'/').unwrap_or(rem.len());
            path = &rem[pos..]
        }

        path.split_once(['?', '#']).map(|p| p.0).unwrap_or(path)
    }

    /// # Reference
    /// [3.4.  Query](https://datatracker.ietf.org/doc/html/rfc3986#section-3.4)
    pub fn query(&self) -> Option<&str> {
        let pos = self.uri.bytes().position(|b| b == b'?' || b == b'#')?;
        if self.uri.as_bytes()[pos] == b'#' {
            return None;
        }
        let query = &self.uri[pos + 1..];
        let pos = query.bytes().position(|b| b == b'#').unwrap_or(query.len());
        Some(&query[..pos])
    }

    /// # Reference
    /// [3.5.  Fragment](https://datatracker.ietf.org/doc/html/rfc3986#section-3.5)
    pub fn fragment(&self) -> Option<&str> {
        let pos = self.uri.bytes().position(|b| b == b'#')?;
        Some(&self.uri[pos + 1..])
    }

    /// Return an iterator that scans the URI components.
    pub fn components(&self) -> Components<'_> {
        Components::new(&self.uri)
    }
}

impl ToOwned for URIStr {
    type Owned = URIString;

    fn to_owned(&self) -> Self::Owned {
        URIString {
            uri: self.uri.to_owned(),
        }
    }
}

impl From<&URIStr> for URIString {
    fn from(value: &URIStr) -> Self {
        value.to_owned()
    }
}

impl AsRef<URIStr> for URIStr {
    fn as_ref(&self) -> &URIStr {
        self
    }
}

impl Clone for Box<URIStr> {
    fn clone(&self) -> Self {
        self.as_ref().into()
    }
}

impl std::fmt::Display for URIStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.as_unescaped_str()
                .as_deref()
                .unwrap_or(self.as_escaped_str())
        )
    }
}

macro_rules! impl_boxed_convertion_uri_str {
    ($( $t:ident ),*) => {
        $(
            impl From<&URIStr> for $t<URIStr> {
                fn from(value: &URIStr) -> Self {
                    let boxed: $t<str> = value.uri.into();
                    unsafe {
                        // # Safety
                        // Since `URIStr` is a transparent newtype of `str`,
                        // the bit patterns are exactly the same and have the same features.
                        std::mem::transmute(boxed)
                    }
                }
            }
        )*
    };
}
impl_boxed_convertion_uri_str!(Box, Rc, Arc);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct URIString {
    /// Escaped URI string.
    ///
    /// Parts generated from UTF-8 strings can always be converted back
    /// to the original UTF-8 byte sequence.
    /// Similarly, the parts generated from Path can probably be converted back
    /// to the original Path byte sequence.
    ///
    /// As a result of resolving URI references, there may be a mixture of parts generated
    /// from UTF-8 strings and parts generated from Paths, so the whole may not always revert
    /// to a UTF-8 string or Path byte sequence.
    uri: String,
}

impl URIString {
    /// Parse the string as a URI by escaping all characters not specified as
    /// [`reserved`](https://datatracker.ietf.org/doc/html/rfc3986#section-2.2)
    /// or [`unreserved`](https://datatracker.ietf.org/doc/html/rfc3986#section-2.3)
    /// in [RFC 3986](https://datatracker.ietf.org/doc/html/rfc3986).
    ///
    /// Because certain characters containing `%` are escaped, the result of
    /// [`URIStr::as_unescaped_str`] is equal to `uri`, but the result of
    /// [`URIStr::as_escaped_str`] may differ from `uri`.
    ///
    /// Since it escapes nearly all characters—including control characters, `%`,
    /// and non-ASCII characters—it will successfully parse any string that roughly
    /// follows URI notation.
    pub fn parse(uri: impl AsRef<str>) -> Result<Self, ParseRIError> {
        fn _parse(uri: &str) -> Result<URIString, ParseRIError> {
            let uri = escape_except(uri, |b| {
                b.is_ascii() && (is_reserved(b as u8) || is_unreserved(b as u8))
            });
            URIString::parse_escaped(&uri)
        }
        _parse(uri.as_ref())
    }

    /// Parse the string as a URI after applying escaping according
    /// to [XML 1.0 "4.2.2 External Entities"](https://www.w3.org/TR/xml/#sec-external-ent).
    ///
    /// Some characters are escaped without escaping the `%` character, so both the result
    /// of [`URIStr::as_unescaped_str`] and the result of [`URIStr::as_escaped_str`] may
    /// differ from `uri`.
    ///
    /// > System identifiers (and other XML strings meant to be used as URI references) may
    /// > contain characters that, according to [IETF RFC 3986], must be escaped before a
    /// > URI can be used to retrieve the referenced resource. The characters to be escaped
    /// > are the control characters #x0 to #x1F and #x7F (most of which cannot appear in
    /// > XML), space #x20, the delimiters '<' #x3C, '>' #x3E and '"' #x22, the unwise
    /// > characters '{' #x7B, '}' #x7D, '|' #x7C, '\' #x5C, '^' #x5E and '`' #x60, as well
    /// > as all characters above #x7F. Since escaping is not always a fully reversible
    /// > process, it MUST be performed only when absolutely necessary and as late as
    /// > possible in a processing chain. In particular, neither the process of converting
    /// > a relative URI to an absolute one nor the process of passing a URI reference to a
    /// > process or software component responsible for dereferencing it SHOULD trigger
    /// > escaping. When escaping does occur, it MUST be performed as follows:
    /// >
    /// > 1. Each character to be escaped is represented in UTF-8 [Unicode] as one or more
    /// >    bytes.
    /// > 2. The resulting bytes are escaped with the URI escaping mechanism (that is,
    /// >    converted to % HH, where HH is the hexadecimal notation of the byte value).
    /// > 3. The original character is replaced by the resulting character sequence.
    pub fn parse_system_id(uri: impl AsRef<str>) -> Result<Self, ParseRIError> {
        fn _parse(uri: &str) -> Result<URIString, ParseRIError> {
            let uri = escape_except(uri, |b| {
                // XML 1.0 "4.2.2 External Entities"
                b.is_ascii()
                    && !matches!(
                        b as u8,
                        0..=0x1F
                            | 0x20
                            | 0x22
                            | 0x3C
                            | 0x3E
                            | 0x5C
                            | 0x5E
                            | 0x60
                            | 0x7B..=0x7D
                            | 0x7F..
                    )
            });
            URIString::parse_escaped(&uri)
        }
        _parse(uri.as_ref())
    }

    /// Parse the string as a URI without performing any escape processing whatsoever.  \
    /// In other words, `uri` is treated as an escaped string.
    ///
    /// Since percent-encoded characters are treated as percent-encoded, the result of
    /// [`URIStr::as_unescaped_str`] may differ from `uri`. On the other hand, since no
    /// escaping is performed at all, the result of [`URIStr::as_escaped_str`] is always
    /// equal to `uri`.
    fn parse_escaped(uri: impl AsRef<str>) -> Result<Self, ParseRIError> {
        fn _parse(uri: &str) -> Result<URIString, ParseRIError> {
            let mut bytes = uri.as_bytes();
            parse_uri_reference(&mut bytes)?;
            if !bytes.is_empty() {
                Err(ParseRIError::NotTermination)
            } else {
                Ok(URIString {
                    uri: uri.to_owned(),
                })
            }
        }
        _parse(uri.as_ref())
    }

    /// # Note
    /// In the current implementation, paths that cannot be converted to UTF-8 strings
    /// cannot be handled.  \
    /// I don't think there will be any problems in most environments, but there may be
    /// some paths that cannot be handled.
    pub fn parse_file_path(path: impl AsRef<Path>) -> Result<Self, ParseRIError> {
        #[cfg(target_family = "unix")]
        fn _parse_file_path(path: &Path) -> Result<URIString, ParseRIError> {
            let mut path_str = path.to_str().ok_or(ParseRIError::Unsupported)?.to_owned();
            if (path.is_dir() || (path.as_os_str().as_encoded_bytes().ends_with(b"\\")))
                && !path_str.ends_with('/')
            {
                path_str.push('/');
            }
            if path.is_absolute() {
                path_str.insert_str(0, "file://");
            }
            URIString::parse(path_str)
        }
        #[cfg(target_family = "windows")]
        fn _parse_file_path(path: &Path) -> Result<URIString, ParseRIError> {
            use std::path::{Component::*, Prefix::*};

            let mut path_str = String::new();
            let mut verbatim = false;
            for comp in path.components() {
                match comp {
                    Prefix(prefix) => match prefix.kind() {
                        Verbatim(root) => {
                            path_str.push_str("file:///");
                            path_str.push_str(
                                &root
                                    .to_str()
                                    .ok_or(ParseRIError::Unsupported)?
                                    .replace('/', "%2F"),
                            );
                            verbatim = true;
                        }
                        VerbatimUNC(server, root) => {
                            path_str.push_str("file://");
                            path_str.push_str(
                                &server
                                    .to_str()
                                    .ok_or(ParseRIError::Unsupported)?
                                    .replace('/', "%2F"),
                            );
                            path_str.push('/');
                            path_str.push_str(
                                &root
                                    .to_str()
                                    .ok_or(ParseRIError::Unsupported)?
                                    .replace('/', "%2F"),
                            );
                            verbatim = true;
                        }
                        VerbatimDisk(letter) => {
                            path_str.push_str("file:");
                            path_str.push(letter as char);
                            path_str.push(':');
                            verbatim = true;
                        }
                        DeviceNS(device) => {
                            path_str.push_str("file:///");
                            path_str.push_str(device.to_str().ok_or(ParseRIError::Unsupported)?);
                        }
                        UNC(server, root) => {
                            path_str.push_str("file://");
                            path_str.push_str(server.to_str().ok_or(ParseRIError::Unsupported)?);
                            path_str.push('/');
                            path_str.push_str(root.to_str().ok_or(ParseRIError::Unsupported)?);
                        }
                        Disk(letter) => {
                            path_str.push_str("file:");
                            path_str.push(letter as char);
                            path_str.push(':');
                        }
                    },
                    RootDir => {}
                    CurDir => {
                        if !path_str.is_empty() {
                            path_str.push_str("/.");
                        } else {
                            path_str.push_str(".");
                        }
                    }
                    ParentDir => {
                        if !path_str.is_empty() {
                            path_str.push_str("/..");
                        } else {
                            path_str.push_str("..")
                        }
                    }
                    Normal(segment) => {
                        if !path_str.is_empty() {
                            path_str.push('/');
                        }
                        let segment = segment.to_str().ok_or(ParseRIError::Unsupported)?;
                        if verbatim {
                            path_str.push_str(&segment.replace('/', "%2F"));
                        } else {
                            path_str.push_str(segment);
                        }
                    }
                }
            }
            if (path.is_dir()
                || (path.as_os_str().as_encoded_bytes().ends_with(b"\\")
                    || (!verbatim && path.as_os_str().as_encoded_bytes().ends_with(b"/"))))
                && !path_str.ends_with('/')
            {
                path_str.push('/');
            }
            URIString::parse(path_str)
        }
        #[cfg(all(not(target_family = "unix"), not(target_family = "windows")))]
        fn _parse_file_path(path: &Path) -> Result<URIString, ParseRIError> {
            todo!()
        }
        _parse_file_path(path.as_ref())
    }

    pub fn into_boxed_uri_str(self) -> Box<URIStr> {
        Box::from(self.as_ref())
    }

    /// # Reference
    /// [6.2.2.  Syntax-Based Normalization](https://datatracker.ietf.org/doc/html/rfc3986#section-6.2.2).
    pub fn normalize(&mut self) {
        use Component::*;

        let mut uri = String::with_capacity(self.uri.len());
        let mut paths = vec![];
        let mut query = None;
        let mut fragment = None;
        let mut has_root = false;
        for comp in self.components() {
            match comp {
                Scheme(scheme) => {
                    uri.push_str(&scheme.to_ascii_lowercase());
                    uri.push(':');
                }
                Authority {
                    userinfo,
                    host,
                    port,
                } => {
                    uri.push_str("//");
                    if let Some(userinfo) = userinfo {
                        uri.push_str(userinfo);
                        uri.push('@');
                    }
                    uri.push_str(host);
                    if let Some(port) = port {
                        uri.push(':');
                        uri.push_str(port);
                    }
                }
                RootSegment => has_root = true,
                Segment(segment) => paths.push(segment),
                Query(q) => query = Some(q),
                Fragment(f) => fragment = Some(f),
            }
        }
        build_normalized_path(paths.into_iter(), has_root, &mut uri);
        if let Some(query) = query {
            uri.push('?');
            uri.push_str(query);
        }
        if let Some(fragment) = fragment {
            uri.push('#');
            uri.push_str(fragment);
        }
        self.uri = uri;
    }
}

impl AsRef<URIStr> for URIString {
    fn as_ref(&self) -> &URIStr {
        URIStr::new(&self.uri)
    }
}

impl Borrow<URIStr> for URIString {
    fn borrow(&self) -> &URIStr {
        self.as_ref()
    }
}

impl Deref for URIString {
    type Target = URIStr;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl std::fmt::Display for URIString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_ref())
    }
}

macro_rules! impl_convertion_uri_string {
    ($( $t:ty ),*) => {
        $(
            impl From<URIString> for $t {
                fn from(value: URIString) -> $t {
                    From::from(value.as_ref())
                }
            }
        )*
    };
}
impl_convertion_uri_string!(Box<URIStr>, Rc<URIStr>, Arc<URIStr>);

fn build_normalized_path<'a>(
    segments: impl Iterator<Item = &'a str>,
    has_root: bool,
    buffer: &mut String,
) {
    let segments = normalize_path_segments(segments, has_root);
    if has_root {
        buffer.push('/');
    }
    for (i, seg) in segments.into_iter().enumerate() {
        if i > 0 {
            buffer.push('/');
        }
        buffer.push_str(seg);
    }
}

fn normalize_path_segments<'a>(
    segments: impl Iterator<Item = &'a str>,
    has_root: bool,
) -> Vec<&'a str> {
    let mut stack = vec![];
    let mut last_dot = false;
    for seg in segments {
        if seg == "." {
            // no op
            last_dot = true;
        } else if seg == ".." {
            if !stack.is_empty() && stack.last() != Some(&"..") {
                stack.pop();
            } else if !has_root {
                stack.push(seg);
            }
            last_dot = true;
        } else {
            stack.push(seg);
            last_dot = false;
        }
    }

    if last_dot {
        stack.push("");
    }

    stack
}

/// # Reference
/// [4.1.  URI Reference](https://datatracker.ietf.org/doc/html/rfc3986#section-4.1)
///
/// ```text
/// URI-reference = URI / relative-ref
/// ```
fn parse_uri_reference(b: &mut &[u8]) -> Result<(), ParseRIError> {
    if b.is_empty() || matches!(b[0], b'/' | b'?' | b'#') {
        // If `b` is an empty string or starts with either '/', '?' or '#',
        // it is definitely 'relative-ref'.
        parse_relative_ref(b)
    } else {
        // Otherwise, it is necessary to distinguish between `URI` and `relative-ref`
        // starting with `relative-part` that matches `path-noscheme`.

        if !b[0].is_ascii_alphabetic() {
            // Since `scheme` begins with at least one `ALPHA`,
            // if it does not, it is definitely `irelative-ref`.
            parse_relative_ref(b)
        } else {
            // The characters that can be used in `scheme` are very limited,
            // so it might be quicker to try parsing `scheme` to distinguish between them?
            // [25] scheme ::= ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
            if let Some(&c) = b
                .iter()
                .find(|&&b| !b.is_ascii_alphanumeric() && !matches!(b, b'+' | b'-' | b'.'))
                && c == b':'
            {
                parse_uri(b)
            } else {
                parse_relative_ref(b)
            }
        }
    }
}

/// # Reference
/// [3.  Syntax Components](https://datatracker.ietf.org/doc/html/rfc3986#section-3)
///
/// ```text
/// URI         = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
/// ```
fn parse_uri(b: &mut &[u8]) -> Result<(), ParseRIError> {
    parse_scheme(b)?;
    *b = b
        .strip_prefix(b":")
        .ok_or(ParseRIError::InvalidSchemeSeparator)?;
    parse_hier_part(b)?;
    if let Some(query) = b.strip_prefix(b"?") {
        *b = query;
        parse_query(b)?;
    }
    if let Some(fragment) = b.strip_prefix(b"#") {
        *b = fragment;
        parse_fragment(b)?;
    }
    Ok(())
}

/// # Reference
/// [3.1.  Scheme](https://datatracker.ietf.org/doc/html/rfc3986#section-3.1)
///
/// ```text
/// scheme      = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
/// ```
fn parse_scheme(b: &mut &[u8]) -> Result<(), ParseRIError> {
    if b.is_empty() || !b[0].is_ascii_alphabetic() {
        return Err(ParseRIError::InvalidScheme);
    }
    let pos = b
        .iter()
        .position(|&b| !b.is_ascii_alphanumeric() && !matches!(b, b'+' | b'-' | b'.'))
        .unwrap_or(b.len());
    *b = &b[pos..];
    Ok(())
}

/// # Reference
/// [3.  Syntax Components](https://datatracker.ietf.org/doc/html/rfc3986#section-3)
///
/// ```text
/// hier-part   = "//" authority path-abempty
///             / path-absolute
///             / path-rootless
///             / path-empty
/// ```
fn parse_hier_part(b: &mut &[u8]) -> Result<(), ParseRIError> {
    if let Some(rem) = b.strip_prefix(b"/") {
        // If `b` starts with '/', `b` starts with 'authority' or `path-absolute`,

        if let Some(rem) = rem.strip_prefix(b"/") {
            // If `b` starts with '//', it should be followed by 'authority'.
            // This is because 'path-absolute' is followed by exactly one '/' at the beginning
            // and optionally 'segment-nz', so there cannot be two consecutive '/' characters.
            *b = rem;
            parse_authority(b)?;
            parse_path_abempty(b)
        } else {
            // path-absolute = "/" [ segment-nz *( "/" segment ) ]
            // segment-nz    = 1*pchar
            parse_path_absolute(b)
        }
    } else {
        // otherwise, `b` starts with 'path-rootless' or 'path-empty'
        let mut dum = *b;
        if parse_pchar(&mut dum).is_ok() {
            // If 'path-rootless' follows, one or more 'pchar' should follow.
            parse_path_rootless(b)
        } else {
            // If not, it is 'path-empty'.
            // Since 'path-empty' is an empty string,
            // we can simply return `Ok` without doing anything.
            Ok(())
        }
    }
}

/// # Reference
/// [3.2.  Authority](https://datatracker.ietf.org/doc/html/rfc3986#section-3.2)
///
/// ```text
/// authority   = [ userinfo "@" ] host [ ":" port ]
/// ```
fn parse_authority(b: &mut &[u8]) -> Result<(), ParseRIError> {
    if b.starts_with(b"[") {
        // If `b` starts with '[', it is definitely an `host` that matches `IP-literal`.
        parse_ip_literal(b)?;
        if let Some(rem) = b.strip_prefix(b":") {
            *b = rem;
            parse_port(b)?;
        }
        return Ok(());
    }

    // If not, it may start with `userinfo`, or it may start with `host`
    // that matches `IPv4address` or `reg-name`.
    //
    // If it is either `IPv4address` or `reg-name`, there is no need to consider `IPv4address`.
    // This is because `reg-name` includes `IPv4address`. More specifically, since `unreserved`
    // contains `DIGIT` and `.`, `IPv4address` can be regarded as a specific sequence of `unreserved`.
    //
    // `userinfo` and `reg-name` are rules that share characters other than colons.
    // Therefore, they can be distinguished using the following algorithm.
    //
    // 1. Increment the counter as long as it matches `userinfo`.
    // 2. If the first ":" is encountered, note its position.
    // 3. Determine the matching rule according to the characters that did not match `userinfo`.
    //      i.   If it is "@", the string seen so far is `userinfo`.
    //      ii.  If it is "[" , then an `host` matching "IP-literal" should start there,
    //           but since there is no "@" immediately before it, it is an error.
    //      iii. In other cases, if the position of ":" is noted, the string before it is `host`;
    //                           if not, all strings seen so far are `host`.
    //
    // userinfo    = *( unreserved / pct-encoded / sub-delims / ":" )
    //
    // reg-name    = *( unreserved / pct-encoded / sub-delims )
    // unreserved  = ALPHA / DIGIT / "-" / "." / "_" / "~"
    //
    // IPv4address = dec-octet "." dec-octet "." dec-octet "." dec-octet
    let mut colon = usize::MAX;
    let mut now = 0;
    let mut t = *b;
    while !t.is_empty() {
        let pos = t
            .iter()
            .position(|&b| !is_unreserved(b) && !is_sub_delims(b) && b != b'%')
            .unwrap_or(t.len());
        t = &t[pos..];
        now += pos;
        if let Some(rem) = t.strip_prefix(b":") {
            now += 1;
            t = rem;
            colon = colon.min(now);
        } else {
            break;
        }
    }

    debug_assert_eq!(now, b.len() - t.len());

    if let Some(rem) = t.strip_prefix(b"@") {
        *b = rem;
        parse_host(b)?;
        if let Some(rem) = b.strip_prefix(b":") {
            *b = rem;
            parse_port(b)?;
        }
        Ok(())
    } else if t.starts_with(b"[") {
        Err(ParseRIError::InvalidAuthority)
    } else if colon < usize::MAX {
        *b = &b[colon + 1..];
        parse_port(b)
    } else {
        *b = t;
        Ok(())
    }
}

// This function has no use.
// /// # Reference
// /// [3.2.1.  User Information](https://datatracker.ietf.org/doc/html/rfc3986#section-3.2.1)
// ///
// /// ```text
// /// userinfo    = *( unreserved / pct-encoded / sub-delims / ":" )
// /// ```
// fn parse_userinfo(b: &mut &[u8]) -> Result<(), ParseRIError> {
//     todo!()
// }

/// # Reference
/// [3.2.2.  Host]
///
/// ```text
/// host        = IP-literal / IPv4address / reg-name
/// ```
fn parse_host(b: &mut &[u8]) -> Result<(), ParseRIError> {
    if b.starts_with(b"[") {
        parse_ip_literal(b)
    } else {
        // Since `IPv4address` is covered by `reg-name`, it does not need to be considered.
        parse_reg_name(b)
    }
}

/// # Reference
/// [3.2.2.  Host]
///
/// ```text
/// IP-literal  = "[" ( IPv6address / IPvFuture  ) "]"
/// ```
fn parse_ip_literal(b: &mut &[u8]) -> Result<(), ParseRIError> {
    *b = b.strip_prefix(b"[").ok_or(ParseRIError::InvalidIPLiteral)?;
    if !b.is_empty() && b[0].eq_ignore_ascii_case(&b'v') {
        parse_ipv_future(b)?;
    } else {
        parse_ipv6_address(b)?;
    }
    *b = b.strip_prefix(b"]").ok_or(ParseRIError::InvalidIPLiteral)?;
    Ok(())
}

/// # Reference
/// [3.2.2.  Host]
///
/// ```text
/// IPvFuture   = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )
/// ```
fn parse_ipv_future(b: &mut &[u8]) -> Result<(), ParseRIError> {
    if b.is_empty() || !b[0].eq_ignore_ascii_case(&b'v') {
        return Err(ParseRIError::InvalidIPvFuture);
    }
    *b = &b[1..];
    let pos = b
        .iter()
        .position(|&b| !b.is_ascii_hexdigit())
        .unwrap_or(b.len());
    if !(1..=b.len() - 2).contains(&pos) {
        return Err(ParseRIError::InvalidIPvFuture);
    }
    *b = &b[pos..];
    *b = b.strip_prefix(b".").ok_or(ParseRIError::InvalidIPvFuture)?;
    let pos = b
        .iter()
        .position(|&b| !is_unreserved(b) && !is_sub_delims(b) && b != b':')
        .unwrap_or(b.len());
    if pos == 0 {
        return Err(ParseRIError::InvalidIPvFuture);
    }
    *b = &b[pos..];
    Ok(())
}

/// # Reference
/// [3.2.2.  Host]
///
/// ```text
/// IPv6address =                            6( h16 ":" ) ls32
///             /                       "::" 5( h16 ":" ) ls32
///             / [               h16 ] "::" 4( h16 ":" ) ls32
///             / [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
///             / [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
///             / [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
///             / [ *4( h16 ":" ) h16 ] "::"              ls32
///             / [ *5( h16 ":" ) h16 ] "::"              h16
///             / [ *6( h16 ":" ) h16 ] "::"
///  ls32       = ( h16 ":" h16 ) / IPv4address
///             ; least-significant 32 bits of address
///  h16        = 1*4HEXDIG
///             ; 16 bits of address represented in hexadecimal
/// ```
fn parse_ipv6_address(b: &mut &[u8]) -> Result<(), ParseRIError> {
    let mut cnt = 1;
    let mut omit = false;
    if let Some(rem) = b.strip_prefix(b":") {
        *b = rem;
        omit = true;
    } else {
        parse_h16(b)?;
    }

    while cnt + (omit as i32) < 8
        && let Some(rem) = b.strip_prefix(b":")
    {
        *b = rem;
        if b.starts_with(b":") {
            if omit {
                return Err(ParseRIError::InvalidIPv6address);
            }
            omit = true;
            cnt += 1;
            continue;
        }

        // It's not a smart approach, but it'll probably work...
        //
        // Checking `h16` first will not work because it cannot be distinguished
        // from the first octet of the IPv4 address.
        //
        // Checking the positions where ':' and '.' appear also seems unlikely to work,
        // considering cases where such characters appear in the segments of the following paths.
        let mut dum = *b;
        if parse_ipv4_address(&mut dum).is_ok() {
            *b = dum;
            // An IPv4 address consumes two hextets.
            cnt += 2;
            // An IPv4 address only appears at the end.
            break;
        } else if !b.is_empty() && b[0].is_ascii_hexdigit() {
            parse_h16(b)?;
        }
    }

    // If "::" is included, some hextets may be omitted, resulting in fewer than eight.
    // Otherwise, exactly eight hextets are required.
    if (omit && cnt <= 8) || (!omit && cnt == 8) {
        Ok(())
    } else {
        Err(ParseRIError::InvalidIPv6address)
    }
}

/// # Reference
/// [3.2.2.  Host]
///
/// ```text
///  h16        = 1*4HEXDIG
///             ; 16 bits of address represented in hexadecimal
/// ```
fn parse_h16(b: &mut &[u8]) -> Result<(), ParseRIError> {
    let pos = b
        .iter()
        .position(|&b| !b.is_ascii_hexdigit())
        .unwrap_or(b.len());
    if pos == 0 {
        Err(ParseRIError::InvalidH16)
    } else {
        *b = &b[pos.min(4)..];
        Ok(())
    }
}

/// # Reference
/// [3.2.2.  Host]
///
/// ```text
/// IPv4address = dec-octet "." dec-octet "." dec-octet "." dec-octet
/// dec-octet   = DIGIT                 ; 0-9
///             / %x31-39 DIGIT         ; 10-99
///             / "1" 2DIGIT            ; 100-199
///             / "2" %x30-34 DIGIT     ; 200-249
///             / "25" %x30-35          ; 250-255
/// ```
fn parse_ipv4_address(b: &mut &[u8]) -> Result<(), ParseRIError> {
    parse_dec_octet(b)?;
    for _ in 0..3 {
        *b = b.strip_prefix(b".").ok_or(ParseRIError::InvalidDecOctet)?;
        parse_dec_octet(b)?;
    }
    Ok(())
}
fn parse_dec_octet(b: &mut &[u8]) -> Result<(), ParseRIError> {
    let len = match b {
        [b'2', b'5', b'0'..=b'5', ..] => 3,
        [b'2', b'0'..=b'4', b'0'..=b'9', ..] => 3,
        [b'1', b'0'..=b'9', b'0'..=b'9', ..] => 3,
        [b'1'..=b'9', b'0'..=b'9', ..] => 2,
        [b'0'..=b'9', ..] => 1,
        _ => return Err(ParseRIError::InvalidDecOctet),
    };
    *b = &b[len..];
    Ok(())
}

/// # Reference
/// [3.2.2.  Host]
///
/// ```text
/// reg-name    = *( unreserved / pct-encoded / sub-delims )
/// ```
fn parse_reg_name(b: &mut &[u8]) -> Result<(), ParseRIError> {
    // pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"
    // reg-name      = pchar - (":" | "@")
    while !b.is_empty() && !matches!(b[0], b':' | b'@') && parse_pchar(b).is_ok() {}
    Ok(())
}

/// # Reference
/// [3.2.3.  Port](https://datatracker.ietf.org/doc/html/rfc3986#section-3.2.3)
///
/// ```text
/// port        = *DIGIT
/// ```
fn parse_port(b: &mut &[u8]) -> Result<(), ParseRIError> {
    let pos = b
        .iter()
        .position(|&b| !b.is_ascii_digit())
        .unwrap_or(b.len());
    *b = &b[pos..];
    Ok(())
}

/// # Reference
/// [3.3.  Path](https://datatracker.ietf.org/doc/html/rfc3986#section-3.3)
///
/// ```text
/// path-abempty  = *( "/" segment )
/// ```
fn parse_path_abempty(b: &mut &[u8]) -> Result<(), ParseRIError> {
    while let Some(rem) = b.strip_prefix(b"/") {
        *b = rem;
        parse_segment(b)?;
    }
    Ok(())
}

/// # Reference
/// [3.3.  Path](https://datatracker.ietf.org/doc/html/rfc3986#section-3.3)
///
/// ```text
/// path-absolute = "/" [ segment-nz *( "/" segment ) ]
/// ```
fn parse_path_absolute(b: &mut &[u8]) -> Result<(), ParseRIError> {
    *b = b
        .strip_prefix(b"/")
        .ok_or(ParseRIError::InvalidPathAbsolute)?;
    if parse_segment_nz(b).is_ok() {
        while let Some(rem) = b.strip_prefix(b"/") {
            *b = rem;
            parse_segment(b)?;
        }
    }
    Ok(())
}

/// # Reference
/// [3.3.  Path](https://datatracker.ietf.org/doc/html/rfc3986#section-3.3)
///
/// ```text
/// path-noscheme = segment-nz-nc *( "/" segment )
/// ```
fn parse_path_noscheme(b: &mut &[u8]) -> Result<(), ParseRIError> {
    parse_segment_nz_nc(b)?;
    while let Some(rem) = b.strip_prefix(b"/") {
        *b = rem;
        parse_segment(b)?;
    }
    Ok(())
}

/// # Reference
/// [3.3.  Path](https://datatracker.ietf.org/doc/html/rfc3986#section-3.3)
///
/// ```text
/// path-rootless = segment-nz *( "/" segment )
/// ```
fn parse_path_rootless(b: &mut &[u8]) -> Result<(), ParseRIError> {
    parse_segment_nz(b)?;
    while let Some(rem) = b.strip_prefix(b"/") {
        *b = rem;
        parse_segment(b)?;
    }
    Ok(())
}

// This is not necessary because this does nothing.
// /// # Reference
// /// [3.3.  Path](https://datatracker.ietf.org/doc/html/rfc3986#section-3.3)
// ///
// /// ```text
// /// path-empty    = 0<pchar>
// /// ```
// fn parse_path_empty(b: &mut &[u8]) -> Result<(), ParseRIError> {
//     todo!()
// }

/// # Reference
/// [3.3.  Path](https://datatracker.ietf.org/doc/html/rfc3986#section-3.3)
///
/// ```text
/// segment       = *pchar
/// ```
fn parse_segment(b: &mut &[u8]) -> Result<(), ParseRIError> {
    while parse_pchar(b).is_ok() {}
    Ok(())
}

/// # Reference
/// [3.3.  Path](https://datatracker.ietf.org/doc/html/rfc3986#section-3.3)
///
/// ```text
/// segment-nz    = 1*pchar
/// ```
fn parse_segment_nz(b: &mut &[u8]) -> Result<(), ParseRIError> {
    parse_pchar(b)?;
    while parse_pchar(b).is_ok() {}
    Ok(())
}

/// # Reference
/// [3.3.  Path](https://datatracker.ietf.org/doc/html/rfc3986#section-3.3)
///
/// ```text
/// segment-nz-nc = 1*( unreserved / pct-encoded / sub-delims / "@" )
///                     ; non-zero-length segment without any colon ":"
/// ```
fn parse_segment_nz_nc(b: &mut &[u8]) -> Result<(), ParseRIError> {
    if b.is_empty() || b[0] == b':' || parse_pchar(b).is_err() {
        return Err(ParseRIError::InvalidSegmentNzNc);
    }
    while !b.is_empty() && b[0] != b':' && parse_pchar(b).is_ok() {}
    Ok(())
}

/// # Reference
/// [3.3.  Path](https://datatracker.ietf.org/doc/html/rfc3986#section-3.3)
///
/// ```text
/// pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"
/// ```
fn parse_pchar(b: &mut &[u8]) -> Result<(), ParseRIError> {
    if b.is_empty() {
        return Err(ParseRIError::InvalidPChar);
    }

    if is_unreserved(b[0]) || is_sub_delims(b[0]) || matches!(b[0], b':' | b'@') {
        *b = &b[1..];
        Ok(())
    } else if b.len() >= 3 && b[0] == b'%' && b[1].is_ascii_hexdigit() && b[2].is_ascii_hexdigit() {
        *b = &b[3..];
        Ok(())
    } else {
        Err(ParseRIError::InvalidPChar)
    }
}

/// # Reference
/// [3.4.  Query](https://datatracker.ietf.org/doc/html/rfc3986#section-3.4)
///
/// ```text
/// query       = *( pchar / "/" / "?" )
/// ```
fn parse_query(b: &mut &[u8]) -> Result<(), ParseRIError> {
    loop {
        if let Some(rem) = b.strip_prefix(b"/") {
            *b = rem;
        } else if let Some(rem) = b.strip_prefix(b"?") {
            *b = rem;
        } else if parse_pchar(b).is_ok() {
            // no op
        } else {
            break Ok(());
        }
    }
}

/// # Reference
/// [3.5.  Fragment](https://datatracker.ietf.org/doc/html/rfc3986#section-3.5)
///
/// ```text
/// fragment    = *( pchar / "/" / "?" )
/// ```
fn parse_fragment(b: &mut &[u8]) -> Result<(), ParseRIError> {
    loop {
        if let Some(rem) = b.strip_prefix(b"/") {
            *b = rem;
        } else if let Some(rem) = b.strip_prefix(b"?") {
            *b = rem;
        } else if parse_pchar(b).is_ok() {
            // no op
        } else {
            break Ok(());
        }
    }
}

/// # Reference
/// [4.2.  Relative Reference](https://datatracker.ietf.org/doc/html/rfc3986#section-4.2)
///
/// ```text
/// relative-ref  = relative-part [ "?" query ] [ "#" fragment ]
/// ```
fn parse_relative_ref(b: &mut &[u8]) -> Result<(), ParseRIError> {
    parse_relative_part(b)?;
    if let Some(query) = b.strip_prefix(b"?") {
        *b = query;
        parse_query(b)?;
    }
    if let Some(fragment) = b.strip_prefix(b"#") {
        *b = fragment;
        parse_fragment(b)?;
    }
    Ok(())
}

/// # Reference
/// [4.2.  Relative Reference](https://datatracker.ietf.org/doc/html/rfc3986#section-4.2)
///
/// ```text
/// relative-part = "//" authority path-abempty
///               / path-absolute
///               / path-noscheme
///               / path-empty
/// ```
fn parse_relative_part(b: &mut &[u8]) -> Result<(), ParseRIError> {
    if let Some(rem) = b.strip_prefix(b"/") {
        if let Some(rem) = rem.strip_prefix(b"/") {
            *b = rem;
            parse_authority(b)?;
            parse_path_abempty(b)
        } else {
            parse_path_absolute(b)
        }
    } else {
        let orig = b.len();
        let ret = parse_path_noscheme(b);
        // If no characters have been consumed, it matches `path-empty` and returns `Ok`.
        if orig == b.len() { Ok(()) } else { ret }
    }
}

/// # Reference
/// [2.2.  Reserved Characters](https://datatracker.ietf.org/doc/html/rfc3986#section-2.2)
///
/// ```text
/// reserved    = gen-delims / sub-delims
/// ```
fn is_reserved(b: u8) -> bool {
    is_gen_delims(b) || is_sub_delims(b)
}

/// # Reference
/// [2.2.  Reserved Characters](https://datatracker.ietf.org/doc/html/rfc3986#section-2.2)
///
/// ```text
/// gen-delims  = ":" / "/" / "?" / "#" / "[" / "]" / "@"
/// ```
fn is_gen_delims(b: u8) -> bool {
    matches!(b, b':' | b'/' | b'?' | b'#' | b'[' | b']' | b'@')
}

/// # Reference
/// [2.2.  Reserved Characters](https://datatracker.ietf.org/doc/html/rfc3986#section-2.2)
///
/// ```text
/// sub-delims  = "!" / "$" / "&" / "'" / "(" / ")" / "*" / "+" / "," / ";" / "="
/// ```
fn is_sub_delims(b: u8) -> bool {
    matches!(
        b,
        b'!' | b'$' | b'&' | b'\'' | b'(' | b')' | b'*' | b'+' | b',' | b';' | b'='
    )
}

/// # Reference
/// [2.3.  Unreserved Characters](https://datatracker.ietf.org/doc/html/rfc3986#section-2.3)
///
/// ```text
/// unreserved  = ALPHA / DIGIT / "-" / "." / "_" / "~"
/// ```
fn is_unreserved(b: u8) -> bool {
    b.is_ascii_alphanumeric() || matches!(b, b'-' | b'.' | b'_' | b'~')
}

const LUT_BYTES: [u8; 256 * 3] = {
    const fn digit_to_hex_char(b: u8) -> u8 {
        if b < 10 { b + b'0' } else { b - 10 + b'A' }
    }
    let mut buf = [0u8; 256 * 3];
    let mut i = 0;
    while i < 256 {
        buf[3 * i] = b'%';
        let hi = (i as u8 >> 4) & 0xF;
        let lo = i as u8 & 0xF;
        buf[3 * i + 1] = digit_to_hex_char(hi);
        buf[3 * i + 2] = digit_to_hex_char(lo);
        i += 1;
    }
    buf
};
const LUT: &str = unsafe {
    // # Safety
    // `LUT_BYTES` contains only '%' and ASCII hex digit characters.
    // Therefore, UTF-8 validation won't fail.
    from_utf8_unchecked(&LUT_BYTES)
};

pub fn escape(s: &str) -> Cow<'_, str> {
    escape_except(s, |_| false)
}

pub fn escape_bytes(b: &[u8]) -> Cow<'_, [u8]> {
    escape_bytes_except(b, |_| false)
}

pub fn escape_except(s: &str, is_except: impl Fn(char) -> bool) -> Cow<'_, str> {
    let cap = s
        .chars()
        .filter_map(|c| (!is_except(c)).then_some(c.len_utf8() * 2))
        .sum::<usize>();
    if cap == 0 {
        return Cow::Borrowed(s);
    }
    let mut encode = [0; 6];
    let mut buf = String::with_capacity(s.len() + cap);
    for c in s.chars() {
        if is_except(c) {
            buf.push(c);
        } else {
            let encoded = c.encode_utf8(&mut encode);
            for b in encoded.bytes() {
                let index = b as usize * 3;
                buf.push_str(&LUT[index..index + 3]);
            }
        }
    }
    Cow::Owned(buf)
}

pub fn escape_bytes_except(b: &[u8], is_except: impl Fn(u8) -> bool) -> Cow<'_, [u8]> {
    let cap = b.iter().copied().filter(|&b| !is_except(b)).count() * 2;
    if cap == 0 {
        return Cow::Borrowed(b);
    }
    let mut buf = Vec::with_capacity(b.len() + cap);
    for &b in b {
        if is_except(b) {
            buf.push(b);
        } else {
            let index = b as usize * 3;
            buf.extend_from_slice(&LUT_BYTES[index..index + 3]);
        }
    }
    Cow::Owned(buf)
}

pub enum URIUnescapeError {
    InvalidEscape,
    Utf8Error(std::str::Utf8Error),
}

impl From<std::str::Utf8Error> for URIUnescapeError {
    fn from(value: std::str::Utf8Error) -> Self {
        Self::Utf8Error(value)
    }
}

pub fn unescape(s: &str) -> Result<Cow<'_, str>, URIUnescapeError> {
    if !s.contains('%') {
        return Ok(Cow::Borrowed(s));
    }

    let mut split = s.split('%');
    let mut buf = String::with_capacity(s.len());
    buf.push_str(split.next().unwrap());
    let mut bytes = vec![];
    for chunk in split {
        if chunk.len() < 2 {
            return Err(URIUnescapeError::InvalidEscape);
        }
        let byte =
            u8::from_str_radix(&chunk[..2], 16).map_err(|_| URIUnescapeError::InvalidEscape)?;
        bytes.push(byte);

        if chunk.len() > 2 {
            buf.push_str(from_utf8(&bytes)?);
            buf.push_str(&chunk[2..]);
            bytes.clear();
        }
    }

    if !bytes.is_empty() {
        buf.push_str(from_utf8(&bytes)?);
    }
    Ok(Cow::Owned(buf))
}

pub fn unescape_bytes(b: &[u8]) -> Result<Cow<'_, [u8]>, URIUnescapeError> {
    if !b.contains(&b'%') {
        return Ok(Cow::Borrowed(b));
    }

    let mut split = b.split(|&b| b == b'%');
    let mut buf = Vec::with_capacity(b.len());
    buf.extend_from_slice(split.next().unwrap());

    fn hexdigit_to_byte(hex: u8) -> u8 {
        if hex.is_ascii_digit() {
            hex - b'0'
        } else if hex.is_ascii_uppercase() {
            hex - b'A' + 10
        } else {
            hex - b'a' + 10
        }
    }
    for chunk in split {
        if chunk.len() < 2 || !chunk[0].is_ascii_hexdigit() || !chunk[1].is_ascii_hexdigit() {
            return Err(URIUnescapeError::InvalidEscape);
        }
        let hi = hexdigit_to_byte(chunk[0]);
        let lo = hexdigit_to_byte(chunk[1]);
        buf.push((hi << 4) | lo);
    }
    Ok(Cow::Owned(buf))
}

#[derive(Debug, Clone, Copy)]
enum DecomposeState {
    Scheme,
    Authority,
    Root,
    Path,
    Query,
    Fragment,
    Finish,
}

pub struct Components<'a> {
    state: DecomposeState,
    uri: &'a str,
}

impl Components<'_> {
    fn new(uri: &str) -> Components<'_> {
        Components {
            state: DecomposeState::Scheme,
            uri,
        }
    }
}

impl<'a> Iterator for Components<'a> {
    type Item = Component<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        use DecomposeState::*;
        loop {
            match self.state {
                Scheme => {
                    self.state = Authority;
                    let mut bytes = self.uri.as_bytes();
                    if parse_scheme(&mut bytes).is_ok() && bytes.starts_with(b":") {
                        let len = self.uri.len() - bytes.len();
                        let (scheme, rem) = self.uri.split_at(len);
                        self.uri = &rem[1..];
                        break Some(Component::Scheme(scheme));
                    }
                }
                Authority => {
                    self.state = Root;
                    if let Some(rem) = self.uri.strip_prefix("//") {
                        let pos = rem.bytes().position(|b| b == b'/').unwrap_or(rem.len());
                        let (mut authority, rem) = rem.split_at(pos);
                        self.uri = rem;
                        let mut userinfo = None;
                        if let Some((ui, rem)) = authority.split_once('@') {
                            userinfo = Some(ui);
                            authority = rem;
                        }
                        let mut port = None;
                        if let Some((host, p)) = authority.rsplit_once(':')
                            && p.bytes().all(|b| b.is_ascii_digit())
                        {
                            port = Some(p);
                            authority = host;
                        }
                        break Some(Component::Authority {
                            userinfo,
                            host: authority,
                            port,
                        });
                    }
                }
                Root => {
                    self.state = Path;
                    if let Some(rem) = self.uri.strip_prefix('/') {
                        self.uri = rem;
                        break Some(Component::RootSegment);
                    }
                }
                Path => {
                    let pos = self
                        .uri
                        .bytes()
                        .position(|b| b == b'/' || b == b'?' || b == b'#')
                        .unwrap_or(self.uri.len());
                    let (segment, rem) = self.uri.split_at(pos);
                    if let Some(rem) = rem.strip_prefix('/') {
                        self.uri = rem;
                    } else {
                        self.uri = rem;
                        self.state = Query;
                    }
                    break Some(Component::Segment(segment));
                }
                Query => {
                    self.state = Fragment;
                    if let Some(rem) = self.uri.strip_prefix('?') {
                        let pos = rem.bytes().position(|b| b == b'#').unwrap_or(rem.len());
                        let (query, rem) = rem.split_at(pos);
                        self.uri = rem;
                        break Some(Component::Query(query));
                    }
                }
                Fragment => {
                    debug_assert!(self.uri.is_empty() || self.uri.starts_with('#'));
                    self.state = Finish;
                    if !self.uri.is_empty() {
                        let (_, frag) = self.uri.split_at(1);
                        self.uri = "";
                        break Some(Component::Fragment(frag));
                    }
                }
                Finish => break None,
            }
        }
    }
}

pub enum Component<'a> {
    Scheme(&'a str),
    Authority {
        userinfo: Option<&'a str>,
        host: &'a str,
        port: Option<&'a str>,
    },
    RootSegment,
    Segment(&'a str),
    Query(&'a str),
    Fragment(&'a str),
}
