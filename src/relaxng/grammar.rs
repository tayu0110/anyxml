use std::collections::{BTreeMap, HashSet};

use crate::{
    error::XMLError,
    relaxng::datatype_library::RelaxNGDatatypeLibraries,
    tree::Element,
    uri::{URIStr, URIString},
};

/// # Reference
/// - ISO/IEC 19757-2:2008 4.2.3 Expressions
///     - Define the relationship between the maximum and minimum values for content types
/// - ISO/IEC 19757-2:2008 10.3 String sequences
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum ContentType {
    Empty = 0,
    Complex = 1,
    Simple = 2,
}

impl ContentType {
    fn groupable(&self, other: ContentType) -> bool {
        match self {
            ContentType::Empty => true,
            ContentType::Complex => matches!(other, ContentType::Empty | ContentType::Complex),
            ContentType::Simple => matches!(other, ContentType::Empty),
        }
    }
}

pub(super) struct RelaxNGGrammar {
    /// If the child of 'start' is 'notAllowed', this field is `None`, otherwise `Some`.
    pub(super) start: Option<RelaxNGPattern>,
    pub(super) define: BTreeMap<String, RelaxNGDefine>,
    pub(super) libraries: RelaxNGDatatypeLibraries,
}

impl RelaxNGGrammar {
    /// # Reference
    /// ISO/IEC 19757-2:2008 10.3 String sequences
    fn verify_content_type(&self) -> Result<(), XMLError> {
        for define in self.define.values() {
            define.verify_content_type()?;
        }
        Ok(())
    }

    /// # Reference
    /// ISO/IEC 19757-2:2008 10.4 Restrictions on attributes
    fn verify_attribute_uniqueness(&self) -> Result<(), XMLError> {
        if let Some(start) = self.start.as_ref() {
            start.verify_attribute_uniqueness(&mut vec![])?;
        }
        for define in self.define.values() {
            define.verify_attribute_uniqueness()?;
        }
        Ok(())
    }

    /// # Reference
    /// ISO/IEC 19757-2:2008 10.4 Restrictions on attributes
    fn verify_attribute_repeat(&self) -> Result<(), XMLError> {
        if let Some(start) = self.start.as_ref() {
            start.verify_attribute_with_infinite_name_class(false)?;
        }
        for define in self.define.values() {
            define.verify_attribute_with_infinite_name_class()?;
        }
        Ok(())
    }

    /// # Reference
    /// ISO/IEC 19757-2:2008 10.5 Restrictions on `interleave`
    fn verify_element_name_uniqueness(&self) -> Result<(), XMLError> {
        if let Some(start) = self.start.as_ref() {
            start.verify_element_name_uniqueness(self, &mut vec![], &mut 0)?;
        }
        for define in self.define.values() {
            define.verify_element_name_uniqueness(self)?;
        }
        Ok(())
    }
}

impl TryFrom<Element> for RelaxNGGrammar {
    type Error = XMLError;

    fn try_from(grammar: Element) -> Result<Self, Self::Error> {
        if grammar.local_name().as_ref() != "grammar" {
            return Err(XMLError::RngParseUnknownError);
        }

        let mut start = None;
        let mut define = BTreeMap::new();
        let mut children = grammar.first_element_child();
        while let Some(element) = children {
            children = element.next_element_sibling();

            match element.local_name().as_ref() {
                "start" => {
                    let top = element
                        .first_element_child()
                        .ok_or(XMLError::RngParseUnknownError)?;
                    if top.local_name().as_ref() != "notAllowed" {
                        start = Some(RelaxNGPattern::try_from(top)?);
                    }
                }
                "define" => {
                    let name = element
                        .get_attribute("name", None)
                        .ok_or(XMLError::RngParseUnknownError)?;
                    define.insert(name, RelaxNGDefine::try_from(element)?);
                }
                _ => return Err(XMLError::RngParseUnknownError),
            }
        }

        let grammar = Self {
            start,
            define,
            libraries: RelaxNGDatatypeLibraries::default(),
        };
        grammar.verify_content_type()?;
        grammar.verify_attribute_uniqueness()?;
        grammar.verify_attribute_repeat()?;
        grammar.verify_element_name_uniqueness()?;

        Ok(grammar)
    }
}

impl std::fmt::Display for RelaxNGGrammar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "<grammar xmlns=\"http://relaxng.org/ns/structure/1.0\"><start>"
        )?;
        if let Some(top) = self.start.as_ref() {
            write!(f, "{top}")?;
        } else {
            write!(f, "<notAllowed/>")?;
        }
        write!(f, "</start>")?;
        for define in self.define.values() {
            write!(f, "{define}")?;
        }
        write!(f, "</grammar>")
    }
}

pub(super) struct RelaxNGDefine {
    pub(super) name: Box<str>,
    pub(super) name_class: RelaxNGNameClass,
    /// If the second child of 'element' is 'notAllowed', this field is `None`, otherwise `Some`.
    pub(super) top: Option<RelaxNGPattern>,
}

impl RelaxNGDefine {
    /// # Reference
    /// ISO/IEC 19757-2:2008 10.3 String sequences
    fn verify_content_type(&self) -> Result<(), XMLError> {
        let top = self
            .top
            .as_ref()
            .ok_or(XMLError::RngParseUngroupablePattern)?;

        if top.verify_content_type().is_some() {
            Ok(())
        } else {
            Err(XMLError::RngParseUngroupablePattern)
        }
    }

    /// # Reference
    /// ISO/IEC 19757-2:2008 10.4 Restrictions on attributes
    fn verify_attribute_uniqueness(&self) -> Result<(), XMLError> {
        if let Some(top) = self.top.as_ref() {
            top.verify_attribute_uniqueness(&mut vec![])
        } else {
            Ok(())
        }
    }

    /// # Reference
    /// ISO/IEC 19757-2:2008 10.4 Restrictions on attributes
    fn verify_attribute_with_infinite_name_class(&self) -> Result<(), XMLError> {
        if let Some(top) = self.top.as_ref() {
            top.verify_attribute_with_infinite_name_class(false)
        } else {
            Ok(())
        }
    }

    /// # Reference
    /// ISO/IEC 19757-2:2008 10.5 Restrictions on `interleave`
    fn verify_element_name_uniqueness(&self, grammar: &RelaxNGGrammar) -> Result<(), XMLError> {
        if let Some(top) = self.top.as_ref() {
            top.verify_element_name_uniqueness(grammar, &mut vec![], &mut 0)
        } else {
            Ok(())
        }
    }
}

impl TryFrom<Element> for RelaxNGDefine {
    type Error = XMLError;

    fn try_from(define: Element) -> Result<Self, Self::Error> {
        let element = define
            .first_element_child()
            .filter(|elem| elem.local_name().as_ref() == "element")
            .ok_or(XMLError::RngParseUnknownError)?;

        let name = define
            .get_attribute("name", None)
            .ok_or(XMLError::RngParseUnknownError)?
            .into();
        let name_class = element
            .first_element_child()
            .ok_or(XMLError::RngParseUnknownError)?;
        let top = element
            .last_element_child()
            .ok_or(XMLError::RngParseUnknownError)?;

        if name_class
            .next_sibling()
            .is_none_or(|next| !top.is_same_node(next))
        {
            return Err(XMLError::RngParseUnknownError);
        }

        if top.local_name().as_ref() == "notAllowed" {
            Ok(Self {
                name,
                name_class: name_class.try_into()?,
                top: None,
            })
        } else {
            Ok(Self {
                name,
                name_class: name_class.try_into()?,
                top: Some(top.try_into()?),
            })
        }
    }
}

impl std::fmt::Display for RelaxNGDefine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "<define name=\"{}\"><element>{}",
            self.name, self.name_class
        )?;
        if let Some(top) = self.top.as_ref() {
            write!(f, "{top}")?
        } else {
            write!(f, "<notAllowed/>")?
        }
        write!(f, "</element></define>")
    }
}

pub(super) struct RelaxNGPattern {
    /// If 'pattern' is 'empty', this field is `None`, otherwise `Some`.
    pub(super) pattern: Option<RelaxNGNonEmptyPattern>,
}

impl RelaxNGPattern {
    /// # Reference
    /// ISO/IEC 19757-2:2008 10.3 String sequences
    fn verify_content_type(&self) -> Option<ContentType> {
        if let Some(pattern) = self.pattern.as_ref() {
            pattern.verify_content_type()
        } else {
            Some(ContentType::Empty)
        }
    }

    /// # Reference
    /// ISO/IEC 19757-2:2008 10.4 Restrictions on attributes
    fn verify_attribute_uniqueness<'a>(
        &'a self,
        name_classes: &mut Vec<&'a RelaxNGNameClass>,
    ) -> Result<(), XMLError> {
        if let Some(pattern) = self.pattern.as_ref() {
            pattern.verify_attribute_uniqueness(name_classes)
        } else {
            Ok(())
        }
    }

    /// # Reference
    /// ISO/IEC 19757-2:2008 10.4 Restrictions on attributes
    fn verify_attribute_with_infinite_name_class(&self, repeat: bool) -> Result<(), XMLError> {
        if let Some(pattern) = self.pattern.as_ref() {
            pattern.verify_attribute_with_infinite_name_class(repeat)
        } else {
            Ok(())
        }
    }

    /// # Reference
    /// ISO/IEC 19757-2:2008 10.5 Restrictions on `interleave`
    fn verify_element_name_uniqueness<'a>(
        &'a self,
        grammar: &'a RelaxNGGrammar,
        name_classes: &mut Vec<&'a RelaxNGNameClass>,
        text_count: &mut i32,
    ) -> Result<(), XMLError> {
        if let Some(pattern) = self.pattern.as_ref() {
            pattern.verify_element_name_uniqueness(grammar, name_classes, text_count)
        } else {
            Ok(())
        }
    }
}

impl TryFrom<Element> for RelaxNGPattern {
    type Error = XMLError;

    fn try_from(value: Element) -> Result<Self, Self::Error> {
        match value.local_name().as_ref() {
            "empty" => Ok(Self { pattern: None }),
            _ => Ok(Self {
                pattern: Some(value.try_into()?),
            }),
        }
    }
}

impl std::fmt::Display for RelaxNGPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(pattern) = self.pattern.as_ref() {
            write!(f, "{pattern}")
        } else {
            write!(f, "<empty/>")
        }
    }
}

pub(super) enum RelaxNGNonEmptyPattern {
    Text,
    Data {
        type_name: Box<str>,
        datatype_library: Box<URIStr>,
        param: Vec<RelaxNGParam>,
        except_pattern: Option<Box<RelaxNGExceptPattern>>,
    },
    Value {
        type_name: Box<str>,
        datatype_library: Box<URIStr>,
        ns: Box<str>,
        value: Box<str>,
    },
    List {
        pattern: Box<RelaxNGPattern>,
    },
    Attribute {
        name_class: RelaxNGNameClass,
        pattern: Box<RelaxNGPattern>,
    },
    Ref {
        name: Box<str>,
    },
    OneOrMore {
        pattern: Box<RelaxNGNonEmptyPattern>,
    },
    Choice {
        left: Box<RelaxNGPattern>,
        right: Box<RelaxNGNonEmptyPattern>,
    },
    Group {
        pattern: [Box<RelaxNGNonEmptyPattern>; 2],
    },
    Interleave {
        pattern: [Box<RelaxNGNonEmptyPattern>; 2],
    },
}

impl RelaxNGNonEmptyPattern {
    /// # Reference
    /// ISO/IEC 19757-2:2008 10.3 String sequences
    fn verify_content_type(&self) -> Option<ContentType> {
        match self {
            Self::Text => Some(ContentType::Complex),
            Self::Data { except_pattern, .. } => {
                if let Some(pattern) = except_pattern {
                    pattern
                        .pattern
                        .verify_content_type()
                        .map(|_| ContentType::Simple)
                } else {
                    Some(ContentType::Simple)
                }
            }
            Self::Value { .. } => Some(ContentType::Simple),
            Self::List { .. } => Some(ContentType::Simple),
            Self::Attribute { pattern, .. } => {
                pattern.verify_content_type().map(|_| ContentType::Empty)
            }
            Self::Ref { .. } => Some(ContentType::Complex),
            Self::OneOrMore { pattern } => pattern
                .verify_content_type()
                .filter(|ct| !matches!(ct, ContentType::Simple)),
            Self::Choice { left, right } => {
                let ct1 = left.verify_content_type()?;
                let ct2 = right.verify_content_type()?;
                Some(ct1.max(ct2))
            }
            Self::Group { pattern } | Self::Interleave { pattern } => {
                let ct1 = pattern[0].verify_content_type()?;
                let ct2 = pattern[1].verify_content_type()?;
                ct1.groupable(ct2).then(|| ct1.max(ct2))
            }
        }
    }

    /// # Reference
    /// ISO/IEC 19757-2:2008 10.4 Restrictions on attributes
    fn verify_attribute_uniqueness<'a>(
        &'a self,
        name_classes: &mut Vec<&'a RelaxNGNameClass>,
    ) -> Result<(), XMLError> {
        match self {
            Self::Text | Self::Data { .. } | Self::Value { .. } | Self::Ref { .. } => Ok(()),
            Self::List { pattern } => {
                // It is necessary to inspect the attributes of list descendants,
                // but since they must not be inherited by ancestors, `name_classes`
                // must not be passed.
                //
                // ```
                // A pattern p1 is defined to occur in a pattern p2 if
                // - p1 is p2, or
                // - p2 is a choice, interleave, group or oneOrMore element
                //   and p1 occurs in one or more children of p2.
                // ```
                pattern.verify_attribute_uniqueness(&mut vec![])
            }
            Self::Attribute { name_class, .. } => {
                name_classes.push(name_class);
                Ok(())
            }
            Self::OneOrMore { pattern } => pattern.verify_attribute_uniqueness(name_classes),
            Self::Choice { left, right } => {
                left.verify_attribute_uniqueness(name_classes)?;
                right.verify_attribute_uniqueness(name_classes)?;
                Ok(())
            }
            Self::Group { pattern } | Self::Interleave { pattern } => {
                let mut buf = vec![];
                pattern[0].verify_attribute_uniqueness(&mut buf)?;
                pattern[1].verify_attribute_uniqueness(&mut buf)?;

                for (i, &l) in buf.iter().enumerate() {
                    for &r in buf.iter().skip(i + 1) {
                        if l.has_non_empty_intersection(r) {
                            return Err(XMLError::RngParseConflictAttributeNameClass);
                        }
                    }
                }

                name_classes.extend(buf);
                Ok(())
            }
        }
    }

    /// # Reference
    /// ISO/IEC 19757-2:2008 10.4 Restrictions on attributes
    fn verify_attribute_with_infinite_name_class(&self, repeat: bool) -> Result<(), XMLError> {
        match self {
            Self::Text | Self::Data { .. } | Self::Value { .. } | Self::Ref { .. } => Ok(()),
            Self::List { pattern } => pattern.verify_attribute_with_infinite_name_class(repeat),
            Self::Attribute {
                name_class,
                pattern,
            } => {
                let has_infinite_name_class = name_class.has_infinite_name_class();
                if has_infinite_name_class && !repeat {
                    Err(XMLError::RngParseUnrepeatedAttributeWithInfiniteNameClass)
                } else if has_infinite_name_class
                    && !matches!(pattern.pattern, Some(RelaxNGNonEmptyPattern::Text))
                {
                    Err(XMLError::RngParseUnacceptablePattern)
                } else {
                    Ok(())
                }
            }
            Self::OneOrMore { pattern } => pattern.verify_attribute_with_infinite_name_class(true),
            Self::Choice { left, right } => {
                left.verify_attribute_with_infinite_name_class(repeat)?;
                right.verify_attribute_with_infinite_name_class(repeat)?;
                Ok(())
            }
            Self::Group { pattern } | Self::Interleave { pattern } => {
                pattern[0].verify_attribute_with_infinite_name_class(repeat)?;
                pattern[1].verify_attribute_with_infinite_name_class(repeat)?;
                Ok(())
            }
        }
    }

    /// # Reference
    /// ISO/IEC 19757-2:2008 10.5 Restrictions on `interleave`
    fn verify_element_name_uniqueness<'a>(
        &'a self,
        grammar: &'a RelaxNGGrammar,
        name_classes: &mut Vec<&'a RelaxNGNameClass>,
        text_count: &mut i32,
    ) -> Result<(), XMLError> {
        match self {
            Self::Text => {
                *text_count += 1;
                Ok(())
            }
            Self::Data { .. } | Self::Value { .. } | Self::Attribute { .. } => Ok(()),
            Self::List { pattern } => {
                // It is necessary to inspect the attributes of list descendants,
                // but since they must not be inherited by ancestors, `name_classes`
                // must not be passed.
                //
                // ```
                // A pattern p1 is defined to occur in a pattern p2 if
                // - p1 is p2, or
                // - p2 is a choice, interleave, group or oneOrMore element
                //   and p1 occurs in one or more children of p2.
                // ```
                pattern.verify_element_name_uniqueness(grammar, &mut vec![], &mut 0)
            }
            Self::Ref { name } => {
                let define = grammar
                    .define
                    .get(name.as_ref())
                    .ok_or(XMLError::RngParseUnknownError)?;
                name_classes.push(&define.name_class);
                Ok(())
            }
            Self::OneOrMore { pattern } => {
                pattern.verify_element_name_uniqueness(grammar, name_classes, text_count)
            }
            Self::Choice { left, right } => {
                left.verify_element_name_uniqueness(grammar, name_classes, text_count)?;
                right.verify_element_name_uniqueness(grammar, name_classes, text_count)?;
                Ok(())
            }
            Self::Group { pattern } => {
                pattern[0].verify_element_name_uniqueness(grammar, name_classes, text_count)?;
                pattern[1].verify_element_name_uniqueness(grammar, name_classes, text_count)?;
                Ok(())
            }
            Self::Interleave { pattern } => {
                let mut buf1 = vec![];
                let mut count1 = 0;
                pattern[0].verify_element_name_uniqueness(grammar, &mut buf1, &mut count1)?;
                let mut buf2 = vec![];
                let mut count2 = 0;
                pattern[1].verify_element_name_uniqueness(grammar, &mut buf2, &mut count2)?;

                if count1 != 0 && count2 != 0 {
                    return Err(XMLError::RngParseConflictAttributeNameClass);
                }
                *text_count += count1 + count2;
                for &l in &buf1 {
                    for &r in &buf2 {
                        if l.has_non_empty_intersection(r) {
                            return Err(XMLError::RngParseConflictAttributeNameClass);
                        }
                    }
                }
                name_classes.extend(buf1);
                name_classes.extend(buf2);
                Ok(())
            }
        }
    }
}

impl TryFrom<Element> for RelaxNGNonEmptyPattern {
    type Error = XMLError;

    fn try_from(value: Element) -> Result<Self, Self::Error> {
        match value.local_name().as_ref() {
            "text" => Ok(Self::Text),
            "data" => {
                let mut param = vec![];
                let mut children = value.first_child();
                while let Some(child) = children.as_ref() {
                    let ch = child.as_element().ok_or(XMLError::RngParseUnknownError)?;
                    match ch.local_name().as_ref() {
                        "param" => param.push(ch.try_into()?),
                        "except" => break,
                        _ => return Err(XMLError::RngParseUnknownError),
                    }

                    children = child.next_sibling();
                }

                let mut except_pattern = None;
                if let Some(except) = children
                    .as_ref()
                    .and_then(|ch| ch.as_element())
                    .filter(|ch| ch.local_name().as_ref() == "except")
                {
                    children = except.next_sibling();
                    except_pattern = Some(Box::new(except.try_into()?));
                }
                if children.is_some() {
                    return Err(XMLError::RngParseUnknownError);
                }

                Ok(Self::Data {
                    type_name: value
                        .get_attribute("type", None)
                        .ok_or(XMLError::RngParseUnknownError)?
                        .into(),
                    datatype_library: URIString::parse(
                        value
                            .get_attribute("datatypeLibrary", None)
                            .ok_or(XMLError::RngParseUnknownError)?,
                    )?
                    .into(),
                    param,
                    except_pattern,
                })
            }
            "value" => Ok(Self::Value {
                type_name: value
                    .get_attribute("type", None)
                    .ok_or(XMLError::RngParseUnknownError)?
                    .into(),
                datatype_library: URIString::parse(
                    value
                        .get_attribute("datatypeLibrary", None)
                        .ok_or(XMLError::RngParseUnknownError)?,
                )?
                .into(),
                ns: value
                    .get_attribute("ns", None)
                    .ok_or(XMLError::RngParseUnknownError)?
                    .into(),
                value: value
                    .first_child()
                    .map(|ch| ch.text_content())
                    .unwrap_or_default()
                    .into(),
            }),
            "list" => Ok(Self::List {
                pattern: Box::new(
                    value
                        .first_child()
                        .and_then(|ch| ch.as_element())
                        .ok_or(XMLError::RngParseUnknownError)?
                        .try_into()?,
                ),
            }),
            "attribute" => Ok(Self::Attribute {
                name_class: value
                    .first_element_child()
                    .ok_or(XMLError::RngParseUnknownError)?
                    .try_into()?,
                pattern: Box::new(
                    value
                        .last_element_child()
                        .ok_or(XMLError::RngParseUnknownError)?
                        .try_into()?,
                ),
            }),
            "ref" => Ok(Self::Ref {
                name: value
                    .get_attribute("name", None)
                    .ok_or(XMLError::RngParseUnknownError)?
                    .into(),
            }),
            "oneOrMore" => Ok(Self::OneOrMore {
                pattern: Box::new(
                    value
                        .first_child()
                        .and_then(|ch| ch.as_element())
                        .ok_or(XMLError::RngParseUnknownError)?
                        .try_into()?,
                ),
            }),
            "choice" => Ok(Self::Choice {
                left: Box::new(
                    value
                        .first_child()
                        .and_then(|ch| ch.as_element())
                        .ok_or(XMLError::RngParseUnknownError)?
                        .try_into()?,
                ),
                right: Box::new(
                    value
                        .last_child()
                        .and_then(|ch| ch.as_element())
                        .ok_or(XMLError::RngParseUnknownError)?
                        .try_into()?,
                ),
            }),
            "group" => Ok(Self::Group {
                pattern: [
                    Box::new(
                        value
                            .first_child()
                            .and_then(|ch| ch.as_element())
                            .ok_or(XMLError::RngParseUnknownError)?
                            .try_into()?,
                    ),
                    Box::new(
                        value
                            .last_child()
                            .and_then(|ch| ch.as_element())
                            .ok_or(XMLError::RngParseUnknownError)?
                            .try_into()?,
                    ),
                ],
            }),
            "interleave" => Ok(Self::Interleave {
                pattern: [
                    Box::new(
                        value
                            .first_child()
                            .and_then(|ch| ch.as_element())
                            .ok_or(XMLError::RngParseUnknownError)?
                            .try_into()?,
                    ),
                    Box::new(
                        value
                            .last_child()
                            .and_then(|ch| ch.as_element())
                            .ok_or(XMLError::RngParseUnknownError)?
                            .try_into()?,
                    ),
                ],
            }),
            _ => Err(XMLError::RngParseUnknownError),
        }
    }
}

impl std::fmt::Display for RelaxNGNonEmptyPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Text => write!(f, "<text/>"),
            Self::Data {
                type_name,
                datatype_library,
                param,
                except_pattern,
            } => {
                write!(
                    f,
                    "<data type=\"{type_name}\" datatypeLibrary=\"{}\">",
                    datatype_library
                        .as_unescaped_str()
                        .as_deref()
                        .unwrap_or(datatype_library.as_escaped_str())
                )?;
                for param in param {
                    write!(f, "{param}")?;
                }
                if let Some(except) = except_pattern {
                    write!(f, "{except}")?;
                }
                write!(f, "</data>")
            }
            Self::Value {
                type_name,
                datatype_library,
                ns,
                value,
            } => {
                write!(
                    f,
                    "<value datatypeLibrary=\"{}\" type=\"{type_name}\" ns=\"{ns}\">{value}</value>",
                    datatype_library
                        .as_unescaped_str()
                        .as_deref()
                        .unwrap_or(datatype_library.as_escaped_str())
                )
            }
            Self::List { pattern } => {
                write!(f, "<list>{pattern}</list>")
            }
            Self::Attribute {
                name_class,
                pattern,
            } => write!(f, "<attribute>{name_class}{pattern}</attribute>"),
            Self::Ref { name } => write!(f, "<ref name=\"{name}\"/>"),
            Self::OneOrMore { pattern } => write!(f, "<oneOrMore>{pattern}</oneOrMore>"),
            Self::Choice { left, right } => write!(f, "<choice>{left}{right}</choice>"),
            Self::Group { pattern } => write!(f, "<group>{}{}</group>", pattern[0], pattern[1]),
            Self::Interleave { pattern } => {
                write!(f, "<interleave>{}{}</interleave>", pattern[0], pattern[1])
            }
        }
    }
}

pub(super) struct RelaxNGParam {
    pub(super) name: Box<str>,
    pub(super) value: Box<str>,
}

impl TryFrom<Element> for RelaxNGParam {
    type Error = XMLError;

    fn try_from(value: Element) -> Result<Self, Self::Error> {
        if value.local_name().as_ref() != "param" {
            return Err(XMLError::RngParseUnknownError);
        }

        Ok(Self {
            name: value
                .get_attribute("name", None)
                .ok_or(XMLError::RngParseUnknownError)?
                .into(),
            value: value
                .first_child()
                .map(|ch| ch.text_content())
                .ok_or(XMLError::RngParseUnknownError)?
                .into(),
        })
    }
}

impl std::fmt::Display for RelaxNGParam {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<param name=\"{}\">{}</param>", self.name, self.value)
    }
}

pub(super) struct RelaxNGExceptPattern {
    pub(super) pattern: RelaxNGPattern,
}

impl TryFrom<Element> for RelaxNGExceptPattern {
    type Error = XMLError;

    fn try_from(value: Element) -> Result<Self, Self::Error> {
        if value.local_name().as_ref() != "except" {
            return Err(XMLError::RngParseUnknownError);
        }

        Ok(Self {
            pattern: value
                .first_child()
                .and_then(|ch| ch.as_element())
                .ok_or(XMLError::RngParseUnknownError)?
                .try_into()?,
        })
    }
}

impl std::fmt::Display for RelaxNGExceptPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<except>{}</except>", self.pattern)
    }
}

pub(super) enum RelaxNGNameClass {
    AnyName {
        except: Option<RelaxNGExceptNameClass>,
    },
    NsName {
        ns: Box<str>,
        except: Option<RelaxNGExceptNameClass>,
    },
    Name {
        ns: Box<str>,
        value: Box<str>,
    },
    Choice {
        name_class: [Box<RelaxNGNameClass>; 2],
    },
}

impl RelaxNGNameClass {
    /// Try to match the pair of `local_name` and `namespace_name` to nameClass.
    ///
    /// This method does not verify if `local_name` is a valid NCName and not verify
    /// `namespace_name` is a valid namespace name.
    ///
    /// If successfully match, return `true`, otherwise return `false`.
    pub(super) fn try_match(&self, local_name: &str, namespace_name: &str) -> bool {
        match self {
            RelaxNGNameClass::AnyName { except } => except
                .as_ref()
                .map(|name_class| !name_class.name_class.try_match(local_name, namespace_name))
                .unwrap_or(true),
            RelaxNGNameClass::NsName { ns, except } => {
                namespace_name == &**ns
                    && except
                        .as_ref()
                        .map(|name_class| {
                            !name_class.name_class.try_match(local_name, namespace_name)
                        })
                        .unwrap_or(true)
            }
            RelaxNGNameClass::Name { ns, value } => {
                namespace_name == &**ns && local_name == &**value
            }
            RelaxNGNameClass::Choice { name_class } => {
                name_class[0].try_match(local_name, namespace_name)
                    || name_class[1].try_match(local_name, namespace_name)
            }
        }
    }

    /// Check if two nameClasses have non-empty intersection of acceptable name set.
    ///
    /// If have, return `true`, otherwise return `false`.
    ///
    /// # Reference
    /// - [Name class analysis](https://relaxng.org/jclark/nameclass.html)
    fn has_non_empty_intersection(&self, other: &Self) -> bool {
        let mut stack = vec![self, other];
        let mut seen = HashSet::new();
        let mut seen_anyname = false;
        let mut representatives = std::iter::from_fn(move || {
            while let Some(now) = stack.pop() {
                match now {
                    RelaxNGNameClass::AnyName { except } => {
                        if let Some(except) = except.as_ref() {
                            stack.push(&except.name_class);
                        }
                        if !seen_anyname {
                            seen_anyname = true;
                            return Some(("*", "*"));
                        }
                    }
                    RelaxNGNameClass::NsName { ns, except } => {
                        if let Some(except) = except.as_ref() {
                            stack.push(&except.name_class);
                        }
                        if seen.insert((ns.as_ref(), "*")) {
                            return Some((ns.as_ref(), "*"));
                        }
                    }
                    RelaxNGNameClass::Name { ns, value } => {
                        if seen.insert((ns.as_ref(), value.as_ref())) {
                            return Some((ns.as_ref(), value.as_ref()));
                        }
                    }
                    RelaxNGNameClass::Choice { name_class } => {
                        stack.push(name_class[0].as_ref());
                        stack.push(name_class[1].as_ref());
                    }
                }
            }
            None
        });

        representatives.any(|(namespace_name, local_name)| {
            self.try_match(local_name, namespace_name)
                && other.try_match(local_name, namespace_name)
        })
    }

    /// Check if `self` or its descendant nameClass is `anyName` or `nsName`.
    fn has_infinite_name_class(&self) -> bool {
        match self {
            Self::AnyName { .. } | Self::NsName { .. } => true,
            Self::Name { .. } => false,
            Self::Choice { name_class } => {
                name_class[0].has_infinite_name_class() || name_class[1].has_infinite_name_class()
            }
        }
    }
}

impl TryFrom<Element> for RelaxNGNameClass {
    type Error = XMLError;

    fn try_from(value: Element) -> Result<Self, Self::Error> {
        match value.local_name().as_ref() {
            "anyName" => Ok(Self::AnyName {
                except: value
                    .first_child()
                    .and_then(|ch| ch.as_element())
                    .map(RelaxNGExceptNameClass::try_from)
                    .transpose()?,
            }),
            "nsName" => Ok(Self::NsName {
                ns: value
                    .get_attribute("ns", None)
                    .ok_or(XMLError::RngParseUnknownError)?
                    .into(),
                except: value
                    .first_child()
                    .and_then(|ch| ch.as_element())
                    .map(RelaxNGExceptNameClass::try_from)
                    .transpose()?,
            }),
            "name" => Ok(Self::Name {
                ns: value
                    .get_attribute("ns", None)
                    .ok_or(XMLError::RngParseUnknownError)?
                    .into(),
                value: value
                    .first_child()
                    .map(|ch| ch.text_content())
                    .ok_or(XMLError::RngParseUnknownError)?
                    .into(),
            }),
            "choice" => Ok(Self::Choice {
                name_class: [
                    Box::new(
                        value
                            .first_child()
                            .and_then(|ch| ch.as_element())
                            .ok_or(XMLError::RngParseUnknownError)?
                            .try_into()?,
                    ),
                    Box::new(
                        value
                            .last_child()
                            .and_then(|ch| ch.as_element())
                            .ok_or(XMLError::RngParseUnknownError)?
                            .try_into()?,
                    ),
                ],
            }),
            _ => Err(XMLError::RngParseUnknownError),
        }
    }
}

impl std::fmt::Display for RelaxNGNameClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::AnyName { except } => {
                write!(f, "<anyName>")?;
                if let Some(except) = except {
                    write!(f, "{except}")?;
                }
                write!(f, "</anyName>")
            }
            Self::NsName { ns, except } => {
                write!(f, "<nsName ns=\"{ns}\">")?;
                if let Some(except) = except {
                    write!(f, "{except}")?;
                }
                write!(f, "</nsName>")
            }
            Self::Name { ns, value } => write!(f, "<name ns=\"{ns}\">{value}</name>"),
            Self::Choice { name_class } => {
                write!(f, "<choice>{}{}</choice>", name_class[0], name_class[1])
            }
        }
    }
}

pub(super) struct RelaxNGExceptNameClass {
    name_class: Box<RelaxNGNameClass>,
}

impl TryFrom<Element> for RelaxNGExceptNameClass {
    type Error = XMLError;

    fn try_from(value: Element) -> Result<Self, Self::Error> {
        if value.local_name().as_ref() != "except" {
            return Err(XMLError::RngParseUnknownError);
        }

        Ok(Self {
            name_class: Box::new(
                value
                    .first_child()
                    .and_then(|ch| ch.as_element())
                    .ok_or(XMLError::RngParseUnknownError)?
                    .try_into()?,
            ),
        })
    }
}

impl std::fmt::Display for RelaxNGExceptNameClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<except>{}</except>", self.name_class)
    }
}
