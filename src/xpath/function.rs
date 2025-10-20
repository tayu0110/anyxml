use std::{borrow::Cow, collections::HashMap, iter::repeat};

use crate::{
    XMLVersion,
    tree::convert::NodeKind,
    xpath::{XPathContext, XPathError, XPathObject},
};

type XPathFunction = fn(&mut XPathContext, usize) -> Result<XPathObject, XPathError>;

pub(super) struct FunctionLibrary {
    map: HashMap<Cow<'static, str>, XPathFunction>,
}

impl FunctionLibrary {
    pub(super) fn get(&self, name: &str) -> Result<XPathFunction, XPathError> {
        self.map
            .get(name)
            .copied()
            .ok_or(XPathError::UnresolvableFunctionName)
    }
}

impl Default for FunctionLibrary {
    fn default() -> Self {
        // XPath core function library
        Self {
            map: HashMap::from([
                // 4.1 Node Set Functions
                (Cow::Borrowed("last"), last as _),
                (Cow::Borrowed("position"), position as _),
                (Cow::Borrowed("count"), count as _),
                (Cow::Borrowed("id"), id as _),
                (Cow::Borrowed("local-name"), local_name as _),
                (Cow::Borrowed("namespace-uri"), namespace_uri as _),
                (Cow::Borrowed("name"), name as _),
                // 4.2 String Functions
                (Cow::Borrowed("string"), string as _),
                (Cow::Borrowed("concat"), concat as _),
                (Cow::Borrowed("starts-with"), starts_with as _),
                (Cow::Borrowed("contains"), contains as _),
                (Cow::Borrowed("substring-before"), substring_before as _),
                (Cow::Borrowed("substring-after"), substring_after as _),
                (Cow::Borrowed("substring"), substring as _),
                (Cow::Borrowed("string-length"), string_length as _),
                (Cow::Borrowed("normalize-space"), normalize_space as _),
                (Cow::Borrowed("translate"), translate as _),
                // 4.3 Boolean Functions
                (Cow::Borrowed("boolean"), boolean as _),
                (Cow::Borrowed("not"), not as _),
                (Cow::Borrowed("true"), r#true as _),
                (Cow::Borrowed("false"), r#false as _),
                (Cow::Borrowed("lang"), lang as _),
                // 4.4 Number Functions
                (Cow::Borrowed("number"), number as _),
                (Cow::Borrowed("sum"), sum as _),
                (Cow::Borrowed("floor"), floor as _),
                (Cow::Borrowed("ceiling"), ceiling as _),
                (Cow::Borrowed("round"), round as _),
            ]),
        }
    }
}

fn last(context: &mut XPathContext, num_args: usize) -> Result<XPathObject, XPathError> {
    if num_args != 0 {
        return Err(XPathError::IncorrectNumberOfArgument);
    }
    Ok(context.size.into())
}

fn position(context: &mut XPathContext, num_args: usize) -> Result<XPathObject, XPathError> {
    if num_args != 0 {
        return Err(XPathError::IncorrectNumberOfArgument);
    }
    Ok(context.position.into())
}

fn count(context: &mut XPathContext, num_args: usize) -> Result<XPathObject, XPathError> {
    if num_args != 1 {
        return Err(XPathError::IncorrectNumberOfArgument);
    }

    let XPathObject::NodeSet(node_set) = context
        .pop_object()
        .ok_or(XPathError::IncorrectNumberOfArgument)?
    else {
        return Err(XPathError::IncorrectArgumentType);
    };

    Ok(node_set.len().into())
}

fn id(context: &mut XPathContext, num_args: usize) -> Result<XPathObject, XPathError> {
    todo!()
}

fn local_name(context: &mut XPathContext, num_args: usize) -> Result<XPathObject, XPathError> {
    if num_args > 1 {
        return Err(XPathError::IncorrectNumberOfArgument);
    }

    let node = if num_args == 1 {
        context
            .pop_object()
            .ok_or(XPathError::IncorrectNumberOfArgument)?
            .as_nodeset()?
            .first()
            .cloned()
    } else {
        context.node.clone()
    };

    let Some(node) = node else {
        return Ok("".into());
    };

    match node.downcast() {
        NodeKind::Element(element) => Ok(element.local_name().into()),
        NodeKind::Attribute(attribute) => Ok(attribute.local_name().into()),
        NodeKind::Namespace(namespace) => {
            Ok(namespace.prefix().as_deref().unwrap_or_default().into())
        }
        NodeKind::ProcessingInstruction(pi) => Ok(pi.target().into()),
        _ => Ok("".into()),
    }
}

fn namespace_uri(context: &mut XPathContext, num_args: usize) -> Result<XPathObject, XPathError> {
    if num_args > 1 {
        return Err(XPathError::IncorrectNumberOfArgument);
    }

    let node = if num_args == 1 {
        context
            .pop_object()
            .ok_or(XPathError::IncorrectNumberOfArgument)?
            .as_nodeset()?
            .first()
            .cloned()
    } else {
        context.node.clone()
    };

    let Some(node) = node else {
        return Ok("".into());
    };

    match node.downcast() {
        NodeKind::Element(element) => Ok(element
            .namespace_name()
            .as_deref()
            .unwrap_or_default()
            .into()),
        NodeKind::Attribute(attribute) => Ok(attribute
            .namespace_name()
            .as_deref()
            .unwrap_or_default()
            .into()),
        _ => Ok("".into()),
    }
}

fn name(context: &mut XPathContext, num_args: usize) -> Result<XPathObject, XPathError> {
    if num_args > 1 {
        return Err(XPathError::IncorrectNumberOfArgument);
    }

    let node = if num_args == 1 {
        context
            .pop_object()
            .ok_or(XPathError::IncorrectNumberOfArgument)?
            .as_nodeset()?
            .first()
            .cloned()
    } else {
        context.node.clone()
    };

    let Some(node) = node else {
        return Ok("".into());
    };

    match node.downcast() {
        NodeKind::Element(element) => Ok(element.name().into()),
        NodeKind::Attribute(attribute) => Ok(attribute.name().into()),
        NodeKind::Namespace(namespace) => {
            Ok(namespace.prefix().as_deref().unwrap_or_default().into())
        }
        NodeKind::ProcessingInstruction(pi) => Ok(pi.target().into()),
        _ => Ok("".into()),
    }
}

fn string(context: &mut XPathContext, num_args: usize) -> Result<XPathObject, XPathError> {
    if num_args > 1 {
        return Err(XPathError::IncorrectNumberOfArgument);
    }

    if num_args == 1 {
        context
            .pop_object()
            .ok_or(XPathError::IncorrectNumberOfArgument)?
            .cast_to_string()
    } else {
        Ok(context
            .node
            .as_ref()
            .map(|node| node.xpath_string_value())
            .unwrap_or_default()
            .into())
    }
}

fn concat(context: &mut XPathContext, num_args: usize) -> Result<XPathObject, XPathError> {
    if num_args < 2 {
        return Err(XPathError::IncorrectNumberOfArgument);
    }

    let mut buf = vec![];
    for _ in 0..num_args {
        let string = context
            .pop_object()
            .ok_or(XPathError::IncorrectNumberOfArgument)?
            .as_string()?;

        buf.push(string);
    }
    Ok(buf.into_iter().rev().collect::<String>().into())
}

fn starts_with(context: &mut XPathContext, num_args: usize) -> Result<XPathObject, XPathError> {
    if num_args != 2 {
        return Err(XPathError::IncorrectNumberOfArgument);
    }

    let second = context
        .pop_object()
        .ok_or(XPathError::IncorrectNumberOfArgument)?
        .as_string()?;
    let first = context
        .pop_object()
        .ok_or(XPathError::IncorrectNumberOfArgument)?
        .as_string()?;

    Ok(first.starts_with(second.as_ref()).into())
}

fn contains(context: &mut XPathContext, num_args: usize) -> Result<XPathObject, XPathError> {
    if num_args != 2 {
        return Err(XPathError::IncorrectNumberOfArgument);
    }

    let second = context
        .pop_object()
        .ok_or(XPathError::IncorrectNumberOfArgument)?
        .as_string()?;
    let first = context
        .pop_object()
        .ok_or(XPathError::IncorrectNumberOfArgument)?
        .as_string()?;

    Ok(first.contains(second.as_ref()).into())
}

fn substring_before(
    context: &mut XPathContext,
    num_args: usize,
) -> Result<XPathObject, XPathError> {
    if num_args != 2 {
        return Err(XPathError::IncorrectNumberOfArgument);
    }

    let second = context
        .pop_object()
        .ok_or(XPathError::IncorrectNumberOfArgument)?
        .as_string()?;
    let first = context
        .pop_object()
        .ok_or(XPathError::IncorrectNumberOfArgument)?
        .as_string()?;

    Ok(first
        .split_once(second.as_ref())
        .map(|ret| ret.0)
        .unwrap_or("")
        .into())
}

fn substring_after(context: &mut XPathContext, num_args: usize) -> Result<XPathObject, XPathError> {
    if num_args != 2 {
        return Err(XPathError::IncorrectNumberOfArgument);
    }

    let second = context
        .pop_object()
        .ok_or(XPathError::IncorrectNumberOfArgument)?
        .as_string()?;
    let first = context
        .pop_object()
        .ok_or(XPathError::IncorrectNumberOfArgument)?
        .as_string()?;

    Ok(first
        .split_once(second.as_ref())
        .map(|ret| ret.1)
        .unwrap_or("")
        .into())
}

fn substring(context: &mut XPathContext, num_args: usize) -> Result<XPathObject, XPathError> {
    if num_args != 2 && num_args != 3 {
        return Err(XPathError::IncorrectNumberOfArgument);
    }

    let third = if num_args == 3 {
        context
            .pop_object()
            .ok_or(XPathError::IncorrectNumberOfArgument)?
            .as_number()?
    } else {
        f64::INFINITY
    };
    let second = context
        .pop_object()
        .ok_or(XPathError::IncorrectNumberOfArgument)?
        .as_number()?;
    let first = context
        .pop_object()
        .ok_or(XPathError::IncorrectNumberOfArgument)?
        .as_string()?;

    if second.is_nan() || third.is_nan() || third.is_sign_negative() {
        // is this correct ??
        return Ok("".into());
    }

    if second.is_infinite() {
        return if second == f64::NEG_INFINITY && third == f64::INFINITY {
            Ok(first.into())
        } else {
            Ok("".into())
        };
    }

    let start = second.round();
    if start > first.len() as f64 {
        return Ok("".into());
    }

    let end = (start + third.round()).max(1.0);
    let start = start.max(1.0);
    let length = (end - start).min(first.len() as f64) as usize;
    Ok(first
        .chars()
        .skip(start as usize - 1)
        .take(length)
        .collect::<Box<str>>()
        .into())
}

fn string_length(context: &mut XPathContext, num_args: usize) -> Result<XPathObject, XPathError> {
    if num_args > 1 {
        return Err(XPathError::IncorrectNumberOfArgument);
    }

    if num_args == 1 {
        let string = context
            .pop_object()
            .ok_or(XPathError::IncorrectNumberOfArgument)?
            .as_string()?;
        Ok(string.chars().count().into())
    } else {
        Ok(context
            .node
            .as_ref()
            .map(|node| node.xpath_string_value().chars().count())
            .unwrap_or_default()
            .into())
    }
}

fn normalize_space(context: &mut XPathContext, num_args: usize) -> Result<XPathObject, XPathError> {
    if num_args > 1 {
        return Err(XPathError::IncorrectNumberOfArgument);
    }

    let string = if num_args == 1 {
        context
            .pop_object()
            .ok_or(XPathError::IncorrectNumberOfArgument)?
            .as_string()?
    } else {
        context
            .node
            .as_ref()
            .map(|node| node.xpath_string_value())
            .unwrap_or_default()
            .into_boxed_str()
    };
    Ok(string
        .split(|c| XMLVersion::default().is_whitespace(c))
        .filter(|c| !c.is_empty())
        .enumerate()
        .fold(
            String::new(),
            |s, (i, v)| {
                if i > 0 { s + " " + v } else { s + v }
            },
        )
        .into())
}

fn translate(context: &mut XPathContext, num_args: usize) -> Result<XPathObject, XPathError> {
    if num_args != 3 {
        return Err(XPathError::IncorrectNumberOfArgument);
    }

    let third = context
        .pop_object()
        .ok_or(XPathError::IncorrectNumberOfArgument)?
        .as_string()?;
    let second = context
        .pop_object()
        .ok_or(XPathError::IncorrectNumberOfArgument)?
        .as_string()?;
    let first = context
        .pop_object()
        .ok_or(XPathError::IncorrectNumberOfArgument)?
        .as_string()?;

    let mut replacement = second
        .chars()
        .zip(third.chars().map(Some).chain(repeat(None)))
        .collect::<Vec<_>>();
    // Stable sorting is required to retain only the first occurrence
    // of duplicate source characters.
    replacement.sort_by_key(|v| v.0);
    replacement.dedup_by_key(|v| v.0);

    let ret = first
        .chars()
        .filter_map(|c| {
            replacement
                .binary_search_by_key(&c, |v| v.0)
                .map(|pos| replacement[pos].1)
                .unwrap_or(Some(c))
        })
        .collect::<Box<str>>();
    Ok(ret.into())
}

fn boolean(context: &mut XPathContext, num_args: usize) -> Result<XPathObject, XPathError> {
    if num_args != 1 {
        return Err(XPathError::IncorrectNumberOfArgument);
    }
    context
        .pop_object()
        .ok_or(XPathError::IncorrectNumberOfArgument)
        .and_then(|object| object.cast_to_boolean())
}

fn not(context: &mut XPathContext, num_args: usize) -> Result<XPathObject, XPathError> {
    if num_args != 1 {
        return Err(XPathError::IncorrectNumberOfArgument);
    }

    let boolean = context
        .pop_object()
        .ok_or(XPathError::IncorrectNumberOfArgument)?
        .as_boolean()?;

    Ok((!boolean).into())
}

fn r#true(_context: &mut XPathContext, num_args: usize) -> Result<XPathObject, XPathError> {
    if num_args != 0 {
        return Err(XPathError::IncorrectNumberOfArgument);
    }
    Ok(true.into())
}

fn r#false(_context: &mut XPathContext, num_args: usize) -> Result<XPathObject, XPathError> {
    if num_args != 0 {
        return Err(XPathError::IncorrectNumberOfArgument);
    }
    Ok(false.into())
}

fn lang(context: &mut XPathContext, num_args: usize) -> Result<XPathObject, XPathError> {
    todo!()
}

fn number(context: &mut XPathContext, num_args: usize) -> Result<XPathObject, XPathError> {
    if num_args > 1 {
        return Err(XPathError::IncorrectNumberOfArgument);
    }

    if num_args == 1 {
        context
            .pop_object()
            .ok_or(XPathError::IncorrectNumberOfArgument)?
            .cast_to_number()
    } else {
        string(context, 0).and_then(|obj| obj.cast_to_number())
    }
}

fn sum(context: &mut XPathContext, num_args: usize) -> Result<XPathObject, XPathError> {
    if num_args != 1 {
        return Err(XPathError::IncorrectNumberOfArgument);
    }

    let node_set = context
        .pop_object()
        .ok_or(XPathError::IncorrectNumberOfArgument)?
        .as_nodeset()
        .map_err(|_| XPathError::IncorrectArgumentType)?;

    let mut ret = 0.0;
    for node in &node_set {
        ret += XPathObject::String(node.xpath_string_value().into()).as_number()?;
    }
    Ok(ret.into())
}

fn floor(context: &mut XPathContext, num_args: usize) -> Result<XPathObject, XPathError> {
    if num_args != 1 {
        return Err(XPathError::IncorrectNumberOfArgument);
    }

    let number = context
        .pop_object()
        .ok_or(XPathError::IncorrectNumberOfArgument)?
        .as_number()?;

    Ok(number.floor().into())
}

fn ceiling(context: &mut XPathContext, num_args: usize) -> Result<XPathObject, XPathError> {
    if num_args != 1 {
        return Err(XPathError::IncorrectNumberOfArgument);
    }

    let number = context
        .pop_object()
        .ok_or(XPathError::IncorrectNumberOfArgument)?
        .as_number()?;

    Ok(number.ceil().into())
}

fn round(context: &mut XPathContext, num_args: usize) -> Result<XPathObject, XPathError> {
    if num_args != 1 {
        return Err(XPathError::IncorrectNumberOfArgument);
    }

    let number = context
        .pop_object()
        .ok_or(XPathError::IncorrectNumberOfArgument)?
        .as_number()?;

    Ok(number.round().into())
}
