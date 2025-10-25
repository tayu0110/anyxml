pub mod uri;

#[derive(Debug, Clone, Copy)]
pub enum ParseRIError {
    InvalidAuthority,
    InvalidPathAbsolute,
    InvalidSegmentNzNc,
    InvalidPChar,
    InvalidScheme,
    InvalidSchemeSeparator,
    InvalidIPLiteral,
    InvalidIPvFuture,
    InvalidIPv6address,
    InvalidH16,
    InvalidLS32,
    InvalidIPv4address,
    InvalidDecOctet,
    InvalidPctEncoded,
    NotTermination,
    Unsupported,
    Unknown,
}

#[cfg(test)]
mod tests {
    use crate::uri::URIString;

    /// [1.1.2.  Examples](https://datatracker.ietf.org/doc/html/rfc3986#section-1.1.2)
    /// [3.  Syntax Components](https://datatracker.ietf.org/doc/html/rfc3986#section-3)
    /// [iri-string crate](https://docs.rs/iri-string/latest/iri_string/index.html)
    #[test]
    fn uri_parse_tests() {
        let uri = URIString::parse(r#"ftp://ftp.is.co.za/rfc/rfc1808.txt"#).unwrap();
        assert_eq!(uri.scheme().unwrap(), "ftp");
        assert_eq!(uri.authority().unwrap(), "ftp.is.co.za");
        assert!(uri.userinfo().is_none());
        assert_eq!(uri.host().unwrap(), "ftp.is.co.za");
        assert!(uri.port().is_none());
        assert_eq!(uri.path(), "/rfc/rfc1808.txt");
        assert!(uri.query().is_none());
        assert!(uri.fragment().is_none());

        let uri = URIString::parse(r#"http://www.ietf.org/rfc/rfc2396.txt"#).unwrap();
        assert_eq!(uri.scheme().unwrap(), "http");
        assert_eq!(uri.authority().unwrap(), "www.ietf.org");
        assert!(uri.userinfo().is_none());
        assert_eq!(uri.host().unwrap(), "www.ietf.org");
        assert!(uri.port().is_none());
        assert_eq!(uri.path(), "/rfc/rfc2396.txt");
        assert!(uri.query().is_none());
        assert!(uri.fragment().is_none());

        let uri = URIString::parse(r#"ldap://[2001:db8::7]/c=GB?objectClass?one"#).unwrap();
        assert_eq!(uri.scheme().unwrap(), "ldap");
        assert_eq!(uri.authority().unwrap(), "[2001:db8::7]");
        assert!(uri.userinfo().is_none());
        assert_eq!(uri.host().unwrap(), "[2001:db8::7]");
        assert!(uri.port().is_none());
        assert_eq!(uri.path(), "/c=GB");
        assert_eq!(uri.query().unwrap(), "objectClass?one");
        assert!(uri.fragment().is_none());

        let uri = URIString::parse(r#"mailto:John.Doe@example.com"#).unwrap();
        assert_eq!(uri.scheme().unwrap(), "mailto");
        assert!(uri.authority().is_none());
        assert!(uri.userinfo().is_none());
        assert!(uri.host().is_none());
        assert!(uri.port().is_none());
        assert_eq!(uri.path(), "John.Doe@example.com");
        assert!(uri.query().is_none());
        assert!(uri.fragment().is_none());

        let uri = URIString::parse(r#"news:comp.infosystems.www.servers.unix"#).unwrap();
        assert_eq!(uri.scheme().unwrap(), "news");
        assert!(uri.authority().is_none());
        assert!(uri.userinfo().is_none());
        assert!(uri.host().is_none());
        assert!(uri.port().is_none());
        assert_eq!(uri.path(), "comp.infosystems.www.servers.unix");
        assert!(uri.query().is_none());
        assert!(uri.fragment().is_none());

        let uri = URIString::parse(r#"tel:+1-816-555-1212"#).unwrap();
        assert_eq!(uri.scheme().unwrap(), "tel");
        assert!(uri.authority().is_none());
        assert!(uri.userinfo().is_none());
        assert!(uri.host().is_none());
        assert!(uri.port().is_none());
        assert_eq!(uri.path(), "+1-816-555-1212");
        assert!(uri.query().is_none());
        assert!(uri.fragment().is_none());

        let uri = URIString::parse(r#"telnet://192.0.2.16:80/"#).unwrap();
        assert_eq!(uri.scheme().unwrap(), "telnet");
        assert_eq!(uri.authority().unwrap(), "192.0.2.16:80");
        assert!(uri.userinfo().is_none());
        assert_eq!(uri.host().unwrap(), "192.0.2.16");
        assert_eq!(uri.port().unwrap(), "80");
        assert_eq!(uri.path(), "/");
        assert!(uri.query().is_none());
        assert!(uri.fragment().is_none());

        let uri =
            URIString::parse(r#"urn:oasis:names:specification:docbook:dtd:xml:4.1.2"#).unwrap();
        assert_eq!(uri.scheme().unwrap(), "urn");
        assert!(uri.authority().is_none());
        assert!(uri.userinfo().is_none());
        assert!(uri.host().is_none());
        assert!(uri.port().is_none());
        assert_eq!(
            uri.path(),
            "oasis:names:specification:docbook:dtd:xml:4.1.2"
        );
        assert!(uri.query().is_none());
        assert!(uri.fragment().is_none());

        let uri =
            URIString::parse(r#"foo://example.com:8042/over/there?name=ferret#nose"#).unwrap();
        assert_eq!(uri.scheme().unwrap(), "foo");
        assert_eq!(uri.authority().unwrap(), "example.com:8042");
        assert!(uri.userinfo().is_none());
        assert_eq!(uri.host().unwrap(), "example.com");
        assert_eq!(uri.port().unwrap(), "8042");
        assert_eq!(uri.path(), "/over/there");
        assert_eq!(uri.query().unwrap(), "name=ferret");
        assert_eq!(uri.fragment().unwrap(), "nose");

        // [iri-string crate](https://docs.rs/iri-string/latest/iri_string/index.html)
        let uri = URIString::parse(r#"foo:"#).unwrap();
        assert_eq!(uri.scheme().unwrap(), "foo");
        assert!(uri.authority().is_none());
        assert!(uri.userinfo().is_none());
        assert!(uri.host().is_none());
        assert!(uri.port().is_none());
        assert_eq!(uri.path(), "");
        assert!(uri.query().is_none());
        assert!(uri.fragment().is_none());

        let uri = URIString::parse(r#"foo:/"#).unwrap();
        assert_eq!(uri.scheme().unwrap(), "foo");
        assert!(uri.authority().is_none());
        assert!(uri.userinfo().is_none());
        assert!(uri.host().is_none());
        assert!(uri.port().is_none());
        assert_eq!(uri.path(), "/");
        assert!(uri.query().is_none());
        assert!(uri.fragment().is_none());

        let uri = URIString::parse(r#"foo://"#).unwrap();
        assert_eq!(uri.scheme().unwrap(), "foo");
        assert_eq!(uri.authority().unwrap(), "");
        assert!(uri.userinfo().is_none());
        assert_eq!(uri.host().unwrap(), "");
        assert!(uri.port().is_none());
        assert_eq!(uri.path(), "");
        assert!(uri.query().is_none());
        assert!(uri.fragment().is_none());

        let uri = URIString::parse(r#"foo:///"#).unwrap();
        assert_eq!(uri.scheme().unwrap(), "foo");
        assert_eq!(uri.authority().unwrap(), "");
        assert!(uri.userinfo().is_none());
        assert_eq!(uri.host().unwrap(), "");
        assert!(uri.port().is_none());
        assert_eq!(uri.path(), "/");
        assert!(uri.query().is_none());
        assert!(uri.fragment().is_none());

        let uri = URIString::parse(r#"foo:////"#).unwrap();
        assert_eq!(uri.scheme().unwrap(), "foo");
        assert_eq!(uri.authority().unwrap(), "");
        assert!(uri.userinfo().is_none());
        assert_eq!(uri.host().unwrap(), "");
        assert!(uri.port().is_none());
        assert_eq!(uri.path(), "//");
        assert!(uri.query().is_none());
        assert!(uri.fragment().is_none());
    }

    /// [5.4.  Reference Resolution Examples](https://datatracker.ietf.org/doc/html/rfc3986#section-5.4)
    #[test]
    fn uri_resolve_tests() {
        let base = URIString::parse(r#"http://a/b/c/d;p?q"#).unwrap();
        assert!(base.is_absolute());

        // 5.4.1.  Normal Examples
        let rel = URIString::parse("g:h").unwrap();
        assert_eq!(base.resolve(&rel).as_escaped_str(), "g:h");
        let rel = URIString::parse("g").unwrap();
        assert_eq!(base.resolve(&rel).as_escaped_str(), "http://a/b/c/g");
        let rel = URIString::parse("./g").unwrap();
        assert_eq!(base.resolve(&rel).as_escaped_str(), "http://a/b/c/g");
        let rel = URIString::parse("g/").unwrap();
        assert_eq!(base.resolve(&rel).as_escaped_str(), "http://a/b/c/g/");
        let rel = URIString::parse("/g").unwrap();
        assert_eq!(base.resolve(&rel).as_escaped_str(), "http://a/g");
        let rel = URIString::parse("//g").unwrap();
        assert_eq!(base.resolve(&rel).as_escaped_str(), "http://g");
        let rel = URIString::parse("?y").unwrap();
        assert_eq!(base.resolve(&rel).as_escaped_str(), "http://a/b/c/d;p?y");
        let rel = URIString::parse("g?y").unwrap();
        assert_eq!(base.resolve(&rel).as_escaped_str(), "http://a/b/c/g?y");
        let rel = URIString::parse("#s").unwrap();
        assert_eq!(base.resolve(&rel).as_escaped_str(), "http://a/b/c/d;p?q#s");
        let rel = URIString::parse("g#s").unwrap();
        assert_eq!(base.resolve(&rel).as_escaped_str(), "http://a/b/c/g#s");
        let rel = URIString::parse("g?y#s").unwrap();
        assert_eq!(base.resolve(&rel).as_escaped_str(), "http://a/b/c/g?y#s");
        let rel = URIString::parse(";x").unwrap();
        assert_eq!(base.resolve(&rel).as_escaped_str(), "http://a/b/c/;x");
        let rel = URIString::parse("g;x").unwrap();
        assert_eq!(base.resolve(&rel).as_escaped_str(), "http://a/b/c/g;x");
        let rel = URIString::parse("g;x?y#s").unwrap();
        assert_eq!(base.resolve(&rel).as_escaped_str(), "http://a/b/c/g;x?y#s");
        let rel = URIString::parse("").unwrap();
        assert_eq!(base.resolve(&rel).as_escaped_str(), "http://a/b/c/d;p?q");
        let rel = URIString::parse(".").unwrap();
        assert_eq!(base.resolve(&rel).as_escaped_str(), "http://a/b/c/");
        let rel = URIString::parse("./").unwrap();
        assert_eq!(base.resolve(&rel).as_escaped_str(), "http://a/b/c/");
        let rel = URIString::parse("..").unwrap();
        assert_eq!(base.resolve(&rel).as_escaped_str(), "http://a/b/");
        let rel = URIString::parse("../").unwrap();
        assert_eq!(base.resolve(&rel).as_escaped_str(), "http://a/b/");
        let rel = URIString::parse("../g").unwrap();
        assert_eq!(base.resolve(&rel).as_escaped_str(), "http://a/b/g");
        let rel = URIString::parse("../..").unwrap();
        assert_eq!(base.resolve(&rel).as_escaped_str(), "http://a/");
        let rel = URIString::parse("../../").unwrap();
        assert_eq!(base.resolve(&rel).as_escaped_str(), "http://a/");
        let rel = URIString::parse("../../g").unwrap();
        assert_eq!(base.resolve(&rel).as_escaped_str(), "http://a/g");

        // [5.4.2.  Abnormal Examples](https://datatracker.ietf.org/doc/html/rfc3986#section-5.4.2)
        let rel = URIString::parse("../../../g").unwrap();
        assert_eq!(base.resolve(&rel).as_escaped_str(), "http://a/g");
        let rel = URIString::parse("../../../../g").unwrap();
        assert_eq!(base.resolve(&rel).as_escaped_str(), "http://a/g");
        let rel = URIString::parse("/./g").unwrap();
        assert_eq!(base.resolve(&rel).as_escaped_str(), "http://a/g");
        let rel = URIString::parse("/../g").unwrap();
        assert_eq!(base.resolve(&rel).as_escaped_str(), "http://a/g");
        let rel = URIString::parse("g.").unwrap();
        assert_eq!(base.resolve(&rel).as_escaped_str(), "http://a/b/c/g.");
        let rel = URIString::parse(".g").unwrap();
        assert_eq!(base.resolve(&rel).as_escaped_str(), "http://a/b/c/.g");
        let rel = URIString::parse("g..").unwrap();
        assert_eq!(base.resolve(&rel).as_escaped_str(), "http://a/b/c/g..");
        let rel = URIString::parse("..g").unwrap();
        assert_eq!(base.resolve(&rel).as_escaped_str(), "http://a/b/c/..g");
        let rel = URIString::parse("./../g").unwrap();
        assert_eq!(base.resolve(&rel).as_escaped_str(), "http://a/b/g");
        let rel = URIString::parse("./g/.").unwrap();
        assert_eq!(base.resolve(&rel).as_escaped_str(), "http://a/b/c/g/");
        let rel = URIString::parse("g/./h").unwrap();
        assert_eq!(base.resolve(&rel).as_escaped_str(), "http://a/b/c/g/h");
        let rel = URIString::parse("g/../h").unwrap();
        assert_eq!(base.resolve(&rel).as_escaped_str(), "http://a/b/c/h");
        let rel = URIString::parse("g;x=1/./y").unwrap();
        assert_eq!(base.resolve(&rel).as_escaped_str(), "http://a/b/c/g;x=1/y");
        let rel = URIString::parse("g;x=1/../y").unwrap();
        assert_eq!(base.resolve(&rel).as_escaped_str(), "http://a/b/c/y");
        let rel = URIString::parse("g?y/./x").unwrap();
        assert_eq!(base.resolve(&rel).as_escaped_str(), "http://a/b/c/g?y/./x");
        let rel = URIString::parse("g?y/../x").unwrap();
        assert_eq!(base.resolve(&rel).as_escaped_str(), "http://a/b/c/g?y/../x");
        let rel = URIString::parse("g#s/./x").unwrap();
        assert_eq!(base.resolve(&rel).as_escaped_str(), "http://a/b/c/g#s/./x");
        let rel = URIString::parse("g#s/../x").unwrap();
        assert_eq!(base.resolve(&rel).as_escaped_str(), "http://a/b/c/g#s/../x");

        // [Testing XML Base Conformance](https://www.w3.org/XML/2006/12/xmlbase-testing.html)
        assert_eq!(
            URIString::parse("http://www.example.org/one/two")
                .unwrap()
                .resolve(&URIString::parse("").unwrap())
                .as_escaped_str(),
            "http://www.example.org/one/two"
        );
        assert_eq!(
            URIString::parse("http://www.example.org/one/two#frag")
                .unwrap()
                .resolve(&URIString::parse("").unwrap())
                .as_escaped_str(),
            "http://www.example.org/one/two"
        );
    }

    #[cfg(target_family = "windows")]
    #[test]
    fn windows_path_test() {
        assert_eq!(
            URIString::parse_file_path("C:\\Windows\\System32\\")
                .unwrap()
                .as_escaped_str(),
            "file:C:/Windows/System32/"
        );
        assert_eq!(
            URIString::parse_file_path("\\\\localhost\\share\\")
                .unwrap()
                .as_escaped_str(),
            "file://localhost/share/"
        );
    }
}
