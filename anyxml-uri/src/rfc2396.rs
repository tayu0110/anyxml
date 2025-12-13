use crate::ParseRIError;

/// ```text
/// URI-reference = [ absoluteURI | relativeURI ] [ "#" fragment ]
/// ```
pub fn validate_rfc2396_uri(mut uri: &str) -> Result<(), ParseRIError> {
    let mut tmp1 = uri;
    let mut tmp2 = uri;
    if parse_rfc2396_absolute_uri(&mut tmp1).is_ok() {
        uri = tmp1;
    } else if parse_rfc2396_absolute_uri(&mut tmp2).is_ok() {
        uri = tmp2;
    }

    if let Some(mut rem) = uri.strip_prefix('#') {
        parse_rfc2396_fragment(&mut rem)?;
        uri = rem;
    }

    if uri.is_empty() {
        Ok(())
    } else {
        Err(ParseRIError::NotTermination)
    }
}

/// ```text
/// absoluteURI   = scheme ":" ( hier_part | opaque_part )
/// ```
pub fn validate_rfc2396_absolute_uri(mut uri: &str) -> Result<(), ParseRIError> {
    parse_rfc2396_absolute_uri(&mut uri)?;
    if uri.is_empty() {
        Ok(())
    } else {
        Err(ParseRIError::NotTermination)
    }
}
fn parse_rfc2396_absolute_uri(uri: &mut &str) -> Result<(), ParseRIError> {
    parse_rfc2396_scheme(uri)?;
    *uri = uri
        .strip_prefix(':')
        .ok_or(ParseRIError::InvalidSchemeSeparator)?;

    if uri.starts_with('/') {
        parse_rfc2396_hier_part(uri)?;
    } else {
        parse_rfc2396_opaque_part(uri)?;
    }
    Ok(())
}

/// ```text
/// relativeURI   = ( net_path | abs_path | rel_path ) [ "?" query ]
/// ```
pub fn validate_rfc2396_relative_uri(mut uri: &str) -> Result<(), ParseRIError> {
    parse_rfc2396_relative_uri(&mut uri)?;
    if uri.is_empty() {
        Ok(())
    } else {
        Err(ParseRIError::NotTermination)
    }
}
fn parse_rfc2396_relative_uri(uri: &mut &str) -> Result<(), ParseRIError> {
    if uri.starts_with("//") {
        parse_rfc2396_net_path(uri)?;
    } else if uri.starts_with('/') {
        parse_rfc2396_abs_path(uri)?;
    } else {
        parse_rfc2396_rel_path(uri)?;
    }

    if let Some(rem) = uri.strip_prefix('?') {
        *uri = rem;
        parse_rfc2396_query(uri)?;
    }
    Ok(())
}

/// ```text
/// hier_part     = ( net_path | abs_path ) [ "?" query ]
/// ```
fn parse_rfc2396_hier_part(uri: &mut &str) -> Result<(), ParseRIError> {
    if uri.starts_with("//") {
        parse_rfc2396_net_path(uri)?;
    } else {
        parse_rfc2396_abs_path(uri)?;
    }

    if let Some(rem) = uri.strip_prefix('?') {
        *uri = rem;
        parse_rfc2396_query(uri)?;
    }
    Ok(())
}

/// ```text
/// opaque_part   = uric_no_slash *uric
/// ```
fn parse_rfc2396_opaque_part(uri: &mut &str) -> Result<(), ParseRIError> {
    parse_rfc2396_uric_no_slash(uri)?;
    let mut tmp = *uri;
    while parse_rfc2396_uric(&mut tmp).is_ok() {
        *uri = tmp;
    }
    Ok(())
}

/// ```text
/// uric_no_slash = unreserved | escaped | ";" | "?" | ":" | "@" |
///                 "&" | "=" | "+" | "$" | ","
/// ```
fn parse_rfc2396_uric_no_slash(uri: &mut &str) -> Result<(), ParseRIError> {
    if let Some(rem) = uri.strip_prefix(|c: char| {
        is_rfc2396_unreserved(c) || matches!(c, ';' | '?' | ':' | '@' | '&' | '=' | '+' | '$' | ',')
    }) {
        *uri = rem;
        Ok(())
    } else {
        parse_rfc2396_escaped(uri)
    }
}

/// ```text
/// net_path      = "//" authority [ abs_path ]
/// ```
fn parse_rfc2396_net_path(uri: &mut &str) -> Result<(), ParseRIError> {
    *uri = uri
        .strip_prefix("//")
        .ok_or(ParseRIError::InvalidAuthority)?;
    parse_rfc2396_authority(uri)?;
    if uri.starts_with('/') {
        parse_rfc2396_abs_path(uri)?;
    }
    Ok(())
}

/// ```text
/// abs_path      = "/"  path_segments
/// ```
fn parse_rfc2396_abs_path(uri: &mut &str) -> Result<(), ParseRIError> {
    *uri = uri
        .strip_prefix('/')
        .ok_or(ParseRIError::InvalidPathAbsolute)?;
    parse_rfc2396_path_segments(uri)?;
    Ok(())
}

/// ```text
/// rel_path      = rel_segment [ abs_path ]
/// ```
fn parse_rfc2396_rel_path(uri: &mut &str) -> Result<(), ParseRIError> {
    parse_rfc2396_rel_segment(uri)?;
    if uri.starts_with('/') {
        parse_rfc2396_abs_path(uri)?;
    }
    Ok(())
}

/// ```text
/// rel_segment   = 1*( unreserved | escaped |
///                     ";" | "@" | "&" | "=" | "+" | "$" | "," )
/// ```
fn parse_rfc2396_rel_segment(uri: &mut &str) -> Result<(), ParseRIError> {
    let len = uri.len();
    *uri = uri.trim_start_matches(|c: char| {
        is_rfc2396_unreserved(c) || matches!(c, ';' | '@' | '&' | '=' | '+' | '$' | ',')
    });
    while parse_rfc2396_escaped(uri).is_ok() {
        *uri = uri.trim_start_matches(|c: char| {
            is_rfc2396_unreserved(c) || matches!(c, ';' | '@' | '&' | '=' | '+' | '$' | ',')
        });
    }
    if len == uri.len() {
        Err(ParseRIError::InvalidSegment)
    } else {
        Ok(())
    }
}

/// ```text
/// scheme        = alpha *( alpha | digit | "+" | "-" | "." )
/// ```
fn parse_rfc2396_scheme(uri: &mut &str) -> Result<(), ParseRIError> {
    *uri = uri
        .strip_prefix(|c: char| c.is_ascii_alphabetic())
        .ok_or(ParseRIError::InvalidScheme)?;
    *uri =
        uri.trim_start_matches(|c: char| c.is_ascii_alphanumeric() || matches!(c, '+' | '-' | '.'));
    Ok(())
}

/// ```text
/// authority     = server | reg_name
/// ```
fn parse_rfc2396_authority(uri: &mut &str) -> Result<(), ParseRIError> {
    let mut tmp = *uri;
    if parse_rfc2396_server(&mut tmp).is_ok() {
        *uri = tmp;
        Ok(())
    } else {
        parse_rfc2396_reg_name(uri)
    }
}

/// ```text
/// reg_name      = 1*( unreserved | escaped | "$" | "," |
///                     ";" | ":" | "@" | "&" | "=" | "+" )
/// ```
fn parse_rfc2396_reg_name(uri: &mut &str) -> Result<(), ParseRIError> {
    let len = uri.len();
    *uri = uri.trim_start_matches(|c: char| {
        is_rfc2396_unreserved(c) || matches!(c, '$' | ',' | ';' | ':' | '@' | '&' | '=' | '+')
    });
    while parse_rfc2396_escaped(uri).is_ok() {
        *uri = uri.trim_start_matches(|c: char| {
            is_rfc2396_unreserved(c) || matches!(c, '$' | ',' | ';' | ':' | '@' | '&' | '=' | '+')
        });
    }
    if len == uri.len() {
        Err(ParseRIError::InvalidAuthority)
    } else {
        Ok(())
    }
}

/// ```text
/// server        = [ [ userinfo "@" ] hostport ]
/// ```
fn parse_rfc2396_server(uri: &mut &str) -> Result<(), ParseRIError> {
    let mut tmp = *uri;
    if parse_rfc2396_userinfo(&mut tmp).is_ok()
        && let Some(mut rem) = tmp.strip_prefix('@')
        && parse_rfc2396_hostport(&mut rem).is_ok()
    {
        *uri = rem;
        Ok(())
    } else {
        parse_rfc2396_hostport(uri)
    }
}

/// ```text
/// userinfo      = *( unreserved | escaped |
///                    ";" | ":" | "&" | "=" | "+" | "$" | "," )
/// ```
fn parse_rfc2396_userinfo(uri: &mut &str) -> Result<(), ParseRIError> {
    *uri = uri.trim_start_matches(|c: char| {
        is_rfc2396_unreserved(c) || matches!(c, ';' | ':' | '&' | '=' | '+' | '$' | ',')
    });
    while parse_rfc2396_escaped(uri).is_ok() {
        *uri = uri.trim_start_matches(|c: char| {
            is_rfc2396_unreserved(c) || matches!(c, ';' | ':' | '&' | '=' | '+' | '$' | ',')
        });
    }
    Ok(())
}

/// ```text
/// hostport      = host [ ":" port ]
/// ```
fn parse_rfc2396_hostport(uri: &mut &str) -> Result<(), ParseRIError> {
    parse_rfc2396_host(uri)?;
    if let Some(mut rem) = uri.strip_prefix(':') {
        parse_rfc2396_port(&mut rem)?;
        *uri = rem;
    }
    Ok(())
}

/// ```text
/// host          = hostname | IPv4address
/// ```
fn parse_rfc2396_host(uri: &mut &str) -> Result<(), ParseRIError> {
    let mut tmp = *uri;
    if parse_rfc2396_ipv4address(&mut tmp).is_ok() {
        *uri = tmp;
        Ok(())
    } else {
        parse_rfc2396_hostname(uri)
    }
}

/// ```text
/// hostname      = *( domainlabel "." ) toplabel [ "." ]
/// ```
fn parse_rfc2396_hostname(uri: &mut &str) -> Result<(), ParseRIError> {
    let mut tmp = *uri;
    while parse_rfc2396_domainlabel(&mut tmp).is_ok()
        && let Some(rem) = tmp.strip_prefix('.')
    {
        tmp = rem;
        *uri = tmp;
    }
    parse_rfc2396_toplabel(uri)?;
    if let Some(rem) = uri.strip_prefix('.') {
        *uri = rem;
    }
    Ok(())
}

/// ```text
/// domainlabel   = alphanum | alphanum *( alphanum | "-" ) alphanum
/// ```
fn parse_rfc2396_domainlabel(uri: &mut &str) -> Result<(), ParseRIError> {
    *uri = uri
        .strip_prefix(|c: char| c.is_ascii_alphanumeric())
        .ok_or(ParseRIError::InvalidAuthority)?;
    *uri = uri.trim_start_matches(|c: char| c.is_ascii_alphanumeric());
    while let Some(rem) = uri
        .trim_start_matches('-')
        .strip_prefix(|c: char| c.is_alphanumeric())
    {
        *uri = rem.trim_start_matches(|c: char| c.is_ascii_alphanumeric());
    }
    Ok(())
}

/// ```text
/// toplabel      = alpha | alpha *( alphanum | "-" ) alphanum
/// ```
fn parse_rfc2396_toplabel(uri: &mut &str) -> Result<(), ParseRIError> {
    *uri = uri
        .strip_prefix(|c: char| c.is_ascii_alphabetic())
        .ok_or(ParseRIError::InvalidAuthority)?;
    *uri = uri.trim_start_matches(|c: char| c.is_ascii_alphanumeric());
    while let Some(rem) = uri
        .trim_start_matches('-')
        .strip_prefix(|c: char| c.is_alphanumeric())
    {
        *uri = rem.trim_start_matches(|c: char| c.is_ascii_alphanumeric());
    }
    Ok(())
}

/// ```text
/// IPv4address   = 1*digit "." 1*digit "." 1*digit "." 1*digit
/// ```
fn parse_rfc2396_ipv4address(uri: &mut &str) -> Result<(), ParseRIError> {
    *uri = uri
        .strip_prefix(|c: char| c.is_ascii_digit())
        .map(|uri| uri.trim_start_matches(|c: char| c.is_ascii_digit()))
        .and_then(|uri| uri.strip_prefix('.'))
        .and_then(|uri| uri.strip_prefix(|c: char| c.is_ascii_digit()))
        .map(|uri| uri.trim_start_matches(|c: char| c.is_ascii_digit()))
        .and_then(|uri| uri.strip_prefix('.'))
        .and_then(|uri| uri.strip_prefix(|c: char| c.is_ascii_digit()))
        .map(|uri| uri.trim_start_matches(|c: char| c.is_ascii_digit()))
        .and_then(|uri| uri.strip_prefix('.'))
        .and_then(|uri| uri.strip_prefix(|c: char| c.is_ascii_digit()))
        .map(|uri| uri.trim_start_matches(|c: char| c.is_ascii_digit()))
        .ok_or(ParseRIError::InvalidIPv4address)?;
    Ok(())
}

/// ```text
/// port          = *digit
/// ```
fn parse_rfc2396_port(uri: &mut &str) -> Result<(), ParseRIError> {
    *uri = uri.trim_start_matches(|c: char| c.is_ascii_digit());
    Ok(())
}

/// ```text
/// path_segments = segment *( "/" segment )
/// ```
fn parse_rfc2396_path_segments(uri: &mut &str) -> Result<(), ParseRIError> {
    parse_rfc2396_segment(uri)?;
    while let Some(mut rem) = uri.strip_prefix('/')
        && parse_rfc2396_segment(&mut rem).is_ok()
    {
        *uri = rem;
    }
    Ok(())
}

/// ```text
/// segment       = *pchar *( ";" param )
/// ```
fn parse_rfc2396_segment(uri: &mut &str) -> Result<(), ParseRIError> {
    let mut tmp = *uri;
    while parse_rfc2396_pchar(&mut tmp).is_ok() {
        *uri = tmp;
    }

    while let Some(mut tmp) = uri.strip_prefix(';')
        && parse_rfc2396_param(&mut tmp).is_ok()
    {
        *uri = tmp;
    }
    Ok(())
}

/// ```text
/// param         = *pchar
/// ```
fn parse_rfc2396_param(uri: &mut &str) -> Result<(), ParseRIError> {
    let mut tmp = *uri;
    while parse_rfc2396_pchar(&mut tmp).is_ok() {
        *uri = tmp;
    }
    Ok(())
}

/// ```text
/// pchar         = unreserved | escaped |
///                 ":" | "@" | "&" | "=" | "+" | "$" | ","
/// ```
fn parse_rfc2396_pchar(uri: &mut &str) -> Result<(), ParseRIError> {
    if let Some(rem) = uri.strip_prefix(|c: char| {
        is_rfc2396_unreserved(c) || matches!(c, ':' | '@' | '&' | '=' | '+' | '$' | ',')
    }) {
        *uri = rem;
        Ok(())
    } else {
        parse_rfc2396_escaped(uri)
    }
}

/// ```text
/// query         = *uric
/// ```
fn parse_rfc2396_query(uri: &mut &str) -> Result<(), ParseRIError> {
    let mut tmp = *uri;
    while parse_rfc2396_uric(&mut tmp).is_ok() {
        *uri = tmp;
    }
    Ok(())
}

/// ```text
/// fragment      = *uric
/// ```
fn parse_rfc2396_fragment(uri: &mut &str) -> Result<(), ParseRIError> {
    let mut tmp = *uri;
    while parse_rfc2396_uric(&mut tmp).is_ok() {
        *uri = tmp;
    }
    Ok(())
}

/// ```text
/// uric          = reserved | unreserved | escaped
/// ```
fn parse_rfc2396_uric(uri: &mut &str) -> Result<(), ParseRIError> {
    let mut tmp = *uri;
    if parse_rfc2396_escaped(&mut tmp).is_ok() {
        *uri = tmp;
    } else if let Some(rem) = uri.strip_prefix(is_rfc2396_reserved) {
        *uri = rem;
    } else {
        *uri = uri
            .strip_prefix(is_rfc2396_unreserved)
            .ok_or(ParseRIError::InvalidPChar)?;
    }
    Ok(())
}

/// ```text
/// reserved      = ";" | "/" | "?" | ":" | "@" | "&" | "=" | "+" |
///                 "$" | ","
/// ```
fn is_rfc2396_reserved(c: char) -> bool {
    matches!(c, ';' | '/' | '?' | ':' | '@' | '&' | '=' | '+' | '$' | ',')
}

/// ```text
/// unreserved    = alphanum | mark
/// ```
fn is_rfc2396_unreserved(c: char) -> bool {
    c.is_ascii_alphanumeric() || is_rfc2396_mark(c)
}

/// ```text
/// mark          = "-" | "_" | "." | "!" | "~" | "*" | "'" |
///                 "(" | ")"
/// ```
fn is_rfc2396_mark(c: char) -> bool {
    matches!(c, '-' | '_' | '.' | '!' | '~' | '*' | '\'' | '(' | ')')
}

/// ```text
/// escaped       = "%" hex hex
/// ```
fn parse_rfc2396_escaped(uri: &mut &str) -> Result<(), ParseRIError> {
    *uri = uri
        .strip_prefix('%')
        .and_then(|uri| uri.strip_prefix(|c: char| c.is_ascii_hexdigit()))
        .and_then(|uri| uri.strip_prefix(|c: char| c.is_ascii_hexdigit()))
        .ok_or(ParseRIError::InvalidPctEncoded)?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// [1.3. Example URI](https://datatracker.ietf.org/doc/html/rfc2396#section-1.3)
    /// [sepctest.xml for RELAX NG](https://gnosis.cx/download/gnosis/xml/relax/spectest.xml)
    #[test]
    fn uri_validate_tests() {
        // RFC2396
        assert!(validate_rfc2396_uri("ftp://ftp.is.co.za/rfc/rfc1808.txt").is_ok());
        assert!(
            validate_rfc2396_uri(
                "gopher://spinaltap.micro.umn.edu/00/Weather/California/Los%20Angeles"
            )
            .is_ok()
        );
        assert!(
            validate_rfc2396_uri("http://www.math.uio.no/faq/compression-faq/part1.html").is_ok()
        );
        assert!(validate_rfc2396_uri("mailto:mduerst@ifi.unizh.ch").is_ok());
        assert!(validate_rfc2396_uri("news:comp.infosystems.www.servers.unix").is_ok());
        assert!(validate_rfc2396_uri("telnet://melvyl.ucop.edu/").is_ok());

        // spectest.xml
        assert!(validate_rfc2396_absolute_uri("foo_bar:xyzzy").is_err());
        assert!(validate_rfc2396_absolute_uri("foobar:xyzzy").is_ok());
        assert!(validate_rfc2396_absolute_uri("http:ok").is_ok());
        assert!(validate_rfc2396_absolute_uri("foo:").is_err());
        assert!(validate_rfc2396_absolute_uri("http://www.example.com/%").is_err());
        assert!(validate_rfc2396_absolute_uri("http://www.example.com/%xx").is_err());
        assert!(validate_rfc2396_absolute_uri("http://www.example.com/%Aa").is_ok());
        assert!(validate_rfc2396_absolute_uri("xyzzy").is_err());
        assert!(validate_rfc2396_absolute_uri("xyzzy#foo:bar").is_err());
        assert!(validate_rfc2396_absolute_uri("xyzzy?foo:bar").is_err());
        assert!(validate_rfc2396_absolute_uri("xyzzy/foo:bar").is_err());
        assert!(validate_rfc2396_absolute_uri("foo:bar").is_ok());
        assert!(validate_rfc2396_absolute_uri("http://www.example.com#xyzzy").is_err());
        assert!(validate_rfc2396_absolute_uri("http://www.example.com#").is_err());
    }
}
