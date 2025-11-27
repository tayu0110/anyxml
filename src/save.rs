use std::fmt::Write;

/// Write `literal` to `f` with quotation (`"` or `'`).
///
/// Any validations for `literal` are not perfomed.
///
/// If `escape` is `true`, escape `"` in `literal` and quote `literal` with `"`.  
/// otherwise, assume that `literal` does not contain ether `'` or `"` at least.
pub(crate) fn write_quoted<'a>(
    f: &mut (impl Write + 'a),
    literal: &str,
    escape: bool,
) -> std::fmt::Result {
    if !literal.contains('"') {
        // does not contain `"`
        write!(f, "\"{literal}\"")
    } else if escape && literal.contains('\'') {
        // contain `"`, and contain `'`
        // since `escape` is true, escape `"` and quote `"`
        write!(f, "\"")?;
        for (i, chunk) in literal.split('"').enumerate() {
            if i > 0 {
                write!(f, "&quot;{chunk}")?;
            } else {
                write!(f, "{chunk}")?;
            }
        }
        write!(f, "\"")
    } else {
        // contain `"`, but does not contain `'`
        write!(f, "'{literal}'")
    }
}

/// Write `data` to `f` with escaping the following characters.
///
/// - `'\r'` (13, 0x0D)
/// - `'"'`  (34, 0x22)
/// - `'&'`  (38, 0x26)
/// - `'''`  (39, 0x27)
/// - `'<'`  (60, 0x3C)
/// - `'>'`  (62, 0x3E)
pub(crate) fn write_escaped_char_data<'a>(
    f: &mut (impl Write + 'a),
    data: &str,
) -> std::fmt::Result {
    let mut next = 0;
    for (i, b) in data.bytes().enumerate() {
        if matches!(b, b'\r' | b'"' | b'&' | b'\'' | b'<' | b'>') {
            f.write_str(&data[next..i])?;
            f.write_str("&#")?;
            f.write_char((b / 10 + b'0') as char)?;
            f.write_char((b % 10 + b'0') as char)?;
            f.write_char(';')?;
            next = i + 1;
        }
    }
    if next < data.len() {
        f.write_str(&data[next..])?;
    }
    Ok(())
}

pub(crate) fn write_escaped_att_value<'a>(
    f: &mut (impl Write + 'a),
    value: &str,
    need_quote: bool,
    quote: &mut Option<char>,
) -> std::fmt::Result {
    let q = quote.unwrap_or_else(|| {
        if !value.contains('"') {
            // does not contain `"`
            '"'
        } else {
            // contains `"`, may be contain `'` or not
            '\''
        }
    });

    if need_quote {
        f.write_char(q)?;
    }
    let mut next = 0;
    for (i, b) in value.bytes().enumerate() {
        if matches!(b, b'\r' | b'&') || b == q as u8 {
            f.write_str(&value[next..i])?;
            f.write_str("&#")?;
            f.write_char((b / 10 + b'0') as char)?;
            f.write_char((b % 10 + b'0') as char)?;
            f.write_char(';')?;
            next = i + 1;
        }
    }
    if next < value.len() {
        f.write_str(&value[next..])?;
    }

    if need_quote {
        f.write_char(q)?;
    }

    *quote = Some(q);
    Ok(())
}
