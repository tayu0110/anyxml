use std::{
    collections::{BTreeMap, HashMap, HashSet},
    sync::Arc,
};

use crate::{
    XMLVersion,
    error::XMLError,
    relaxng::{
        datatype_library::RelaxNGDatatypeLibraries,
        parse::{ChoiceType, ExceptType, RelaxNGNodeType, RelaxNGParseHandler},
    },
    sax::{
        AttributeType, DefaultDecl, Locator, NamespaceStack,
        attributes::Attributes,
        contentspec::ContentSpec,
        error::SAXParseError,
        handler::{EntityResolver, ErrorHandler, SAXHandler},
        source::InputSource,
    },
    uri::{URIStr, URIString},
};

pub(super) struct RelaxNGGrammar {
    /// If the child of 'start' is 'notAllowed', this field is `None`, otherwise `Some`.
    pub(super) start: Option<RelaxNGPattern>,
    pub(super) define: BTreeMap<String, RelaxNGDefine>,
    pub(super) libraries: RelaxNGDatatypeLibraries,
}

impl RelaxNGGrammar {
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

impl std::fmt::Display for RelaxNGParam {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<param name=\"{}\">{}</param>", self.name, self.value)
    }
}

pub(super) struct RelaxNGExceptPattern {
    pub(super) pattern: RelaxNGPattern,
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

impl std::fmt::Display for RelaxNGExceptNameClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<except>{}</except>", self.name_class)
    }
}

impl<H: SAXHandler> RelaxNGParseHandler<H> {
    pub(super) fn build_grammar(&self) -> Result<RelaxNGGrammar, XMLError> {
        self.do_build_grammar(0)
    }
    fn do_build_grammar(&self, current: usize) -> Result<RelaxNGGrammar, XMLError> {
        assert!(matches!(
            self.tree[current].r#type,
            RelaxNGNodeType::Grammar
        ));
        match self.tree[current].r#type {
            RelaxNGNodeType::Grammar => {
                let mut start = None;
                let mut define = BTreeMap::new();
                for &ch in &self.tree[current].children {
                    match self.tree[ch].r#type {
                        RelaxNGNodeType::Start(_) => {
                            let gch = self.tree[ch].children[0];
                            if !matches!(self.tree[gch].r#type, RelaxNGNodeType::NotAllowed) {
                                start = Some(self.build_pattern(gch)?);
                            }
                        }
                        RelaxNGNodeType::Define { ref name, .. } => {
                            define.insert(name.to_string(), self.build_define(ch)?);
                        }
                        _ => return Err(XMLError::RngParseUnknownError),
                    }
                }

                let grammar = RelaxNGGrammar {
                    start,
                    define,
                    libraries: self.datatype_libraries.clone(),
                };
                grammar.verify_attribute_uniqueness()?;
                grammar.verify_attribute_repeat()?;
                grammar.verify_element_name_uniqueness()?;

                Ok(grammar)
            }
            _ => Err(XMLError::RngParseUnknownError),
        }
    }

    fn build_pattern(&self, current: usize) -> Result<RelaxNGPattern, XMLError> {
        match self.tree[current].r#type {
            RelaxNGNodeType::Empty => Ok(RelaxNGPattern { pattern: None }),
            _ => Ok(RelaxNGPattern {
                pattern: Some(self.build_non_empty_pattern(current)?),
            }),
        }
    }

    fn build_define(&self, current: usize) -> Result<RelaxNGDefine, XMLError> {
        assert!(matches!(
            self.tree[current].r#type,
            RelaxNGNodeType::Define { .. }
        ));
        let element = self.tree[current].children[0];
        match self.tree[current].r#type {
            RelaxNGNodeType::Define { ref name, .. } => {
                let name_class = self.tree[element].children[0];
                let top = self.tree[element].children[1];
                match self.tree[top].r#type {
                    RelaxNGNodeType::NotAllowed => Ok(RelaxNGDefine {
                        name: name.clone(),
                        name_class: self.build_name_class(name_class)?,
                        top: None,
                    }),
                    _ => Ok(RelaxNGDefine {
                        name: name.clone(),
                        name_class: self.build_name_class(name_class)?,
                        top: Some(self.build_pattern(top)?),
                    }),
                }
            }
            _ => Err(XMLError::RngParseUnknownError),
        }
    }

    fn build_non_empty_pattern(&self, current: usize) -> Result<RelaxNGNonEmptyPattern, XMLError> {
        match self.tree[current].r#type {
            RelaxNGNodeType::Text => Ok(RelaxNGNonEmptyPattern::Text),
            RelaxNGNodeType::Data(ref r#type) => {
                let mut param = vec![];
                let mut except_pattern = None;
                for &ch in &self.tree[current].children {
                    match &self.tree[ch].r#type {
                        RelaxNGNodeType::Param { name, value } => {
                            param.push(RelaxNGParam {
                                name: name.clone(),
                                value: value.clone(),
                            });
                        }
                        RelaxNGNodeType::Except(_) => {
                            except_pattern = Some(Box::new(self.build_except_pattern(ch)?))
                        }
                        _ => return Err(XMLError::RngParseUnknownError),
                    }
                }

                Ok(RelaxNGNonEmptyPattern::Data {
                    type_name: r#type.clone(),
                    datatype_library: URIString::parse(
                        self.tree[current]
                            .datatype_library
                            .as_deref()
                            .unwrap_or_default(),
                    )?
                    .into(),
                    param,
                    except_pattern,
                })
            }
            RelaxNGNodeType::Value {
                ref r#type,
                ref value,
                ..
            } => Ok(RelaxNGNonEmptyPattern::Value {
                type_name: r#type.clone().unwrap(),
                datatype_library: URIString::parse(
                    self.tree[current]
                        .datatype_library
                        .as_deref()
                        .unwrap_or_default(),
                )?
                .into(),
                ns: self.tree[current].ns.as_deref().unwrap_or_default().into(),
                value: value.clone(),
            }),
            RelaxNGNodeType::List => {
                let ch = self.tree[current].children[0];
                Ok(RelaxNGNonEmptyPattern::List {
                    pattern: Box::new(self.build_pattern(ch)?),
                })
            }
            RelaxNGNodeType::Attribute(_) => {
                let name_class = self.tree[current].children[0];
                let pattern = self.tree[current].children[1];
                Ok(RelaxNGNonEmptyPattern::Attribute {
                    name_class: self.build_name_class(name_class)?,
                    pattern: Box::new(self.build_pattern(pattern)?),
                })
            }
            RelaxNGNodeType::Ref(ref name) => {
                Ok(RelaxNGNonEmptyPattern::Ref { name: name.clone() })
            }
            RelaxNGNodeType::OneOrMore => {
                let ch = self.tree[current].children[0];
                Ok(RelaxNGNonEmptyPattern::OneOrMore {
                    pattern: Box::new(self.build_non_empty_pattern(ch)?),
                })
            }
            RelaxNGNodeType::Choice(_) => {
                let ch1 = self.tree[current].children[0];
                let ch2 = self.tree[current].children[1];
                Ok(RelaxNGNonEmptyPattern::Choice {
                    left: Box::new(self.build_pattern(ch1)?),
                    right: Box::new(self.build_non_empty_pattern(ch2)?),
                })
            }
            RelaxNGNodeType::Group => {
                let ch1 = self.tree[current].children[0];
                let ch2 = self.tree[current].children[1];
                Ok(RelaxNGNonEmptyPattern::Group {
                    pattern: [
                        Box::new(self.build_non_empty_pattern(ch1)?),
                        Box::new(self.build_non_empty_pattern(ch2)?),
                    ],
                })
            }
            RelaxNGNodeType::Interleave => {
                let ch1 = self.tree[current].children[0];
                let ch2 = self.tree[current].children[1];
                Ok(RelaxNGNonEmptyPattern::Interleave {
                    pattern: [
                        Box::new(self.build_non_empty_pattern(ch1)?),
                        Box::new(self.build_non_empty_pattern(ch2)?),
                    ],
                })
            }
            _ => Err(XMLError::RngParseUnknownError),
        }
    }

    fn build_name_class(&self, current: usize) -> Result<RelaxNGNameClass, XMLError> {
        match self.tree[current].r#type {
            RelaxNGNodeType::AnyName => Ok(RelaxNGNameClass::AnyName {
                except: self.tree[current]
                    .children
                    .first()
                    .map(|&ch| self.build_except_name_class(ch))
                    .transpose()?,
            }),
            RelaxNGNodeType::NsName => Ok(RelaxNGNameClass::NsName {
                ns: self.tree[current].ns.as_deref().unwrap_or_default().into(),
                except: self.tree[current]
                    .children
                    .first()
                    .map(|&ch| self.build_except_name_class(ch))
                    .transpose()?,
            }),
            RelaxNGNodeType::Name(ref name) => Ok(RelaxNGNameClass::Name {
                ns: self.tree[current].ns.as_deref().unwrap_or_default().into(),
                value: name.clone(),
            }),
            RelaxNGNodeType::Choice(_) => {
                let ch1 = self.tree[current].children[0];
                let ch2 = self.tree[current].children[1];
                Ok(RelaxNGNameClass::Choice {
                    name_class: [
                        Box::new(self.build_name_class(ch1)?),
                        Box::new(self.build_name_class(ch2)?),
                    ],
                })
            }
            _ => Err(XMLError::RngParseUnknownError),
        }
    }

    fn build_except_pattern(&self, current: usize) -> Result<RelaxNGExceptPattern, XMLError> {
        assert!(matches!(
            self.tree[current].r#type,
            RelaxNGNodeType::Except(_)
        ));
        match self.tree[current].r#type {
            RelaxNGNodeType::Except(_) => {
                let ch = self.tree[current].children[0];
                Ok(RelaxNGExceptPattern {
                    pattern: self.build_pattern(ch)?,
                })
            }
            _ => Err(XMLError::RngParseUnknownError),
        }
    }

    fn build_except_name_class(&self, current: usize) -> Result<RelaxNGExceptNameClass, XMLError> {
        assert!(matches!(
            self.tree[current].r#type,
            RelaxNGNodeType::Except(_)
        ));
        match self.tree[current].r#type {
            RelaxNGNodeType::Except(_) => {
                let ch = self.tree[current].children[0];
                Ok(RelaxNGExceptNameClass {
                    name_class: Box::new(self.build_name_class(ch)?),
                })
            }
            _ => Err(XMLError::RngParseUnknownError),
        }
    }
}

type Uri = Arc<str>;
type LocalName = Arc<str>;
type ParamList = BTreeMap<LocalName, Arc<str>>;
type Prefix = Arc<str>;
type Context = (Arc<URIStr>, BTreeMap<Prefix, Uri>);
type Datatype = (Uri, LocalName);
type PatternId = usize;

#[derive(PartialEq, Eq, Hash)]
pub(super) enum NameClass {
    AnyName,
    AnyNameExcept(Arc<NameClass>),
    Name(Uri, LocalName),
    NsName(Uri),
    NsNameExcept(Uri, Arc<NameClass>),
    NameClassChoice(Arc<NameClass>, Arc<NameClass>),
}

impl NameClass {
    fn contains(&self, qname: &QName) -> bool {
        match (self, qname) {
            (Self::AnyName, _) => true,
            (Self::AnyNameExcept(nc), n) => !nc.contains(n),
            (Self::NsName(ns1), QName(ns2, _)) => ns1 == ns2,
            (Self::NsNameExcept(ns1, nc), n @ QName(ns2, _)) => ns1 == ns2 && !nc.contains(n),
            (Self::Name(ns1, ln1), QName(ns2, ln2)) => ns1 == ns2 && ln1 == ln2,
            (Self::NameClassChoice(nc1, nc2), n) => nc1.contains(n) || nc2.contains(n),
        }
    }

    fn is_infinite(&self) -> bool {
        match self {
            Self::AnyName | Self::AnyNameExcept(_) | Self::NsName(_) | Self::NsNameExcept(_, _) => {
                true
            }
            Self::Name(_, _) => false,
            Self::NameClassChoice(nc1, nc2) => nc1.is_infinite() || nc2.is_infinite(),
        }
    }

    /// # Reference
    /// - [Name class analysis](https://relaxng.org/jclark/nameclass.html)
    fn overlap(&self, other: &Self) -> bool {
        self.representatives(other)
            .map(|(ns, ln)| QName(ns, ln))
            .any(|qn| self.contains(&qn) && other.contains(&qn))
    }
    /// # Reference
    /// - [Name class analysis](https://relaxng.org/jclark/nameclass.html)
    fn representatives<'a>(
        &'a self,
        other: &'a Self,
    ) -> impl Iterator<Item = (Arc<str>, Arc<str>)> + 'a {
        let mut stack = vec![self, other];
        let mut seen = HashSet::new();
        let mut seen_anyname = false;
        let illegal_local_name = Arc::<str>::from("");
        let illegal_uri = Arc::<str>::from("\x01");
        std::iter::from_fn(move || {
            while let Some(now) = stack.pop() {
                match now {
                    NameClass::AnyName => {
                        if !seen_anyname {
                            seen_anyname = true;
                            return Some((illegal_uri.clone(), illegal_local_name.clone()));
                        }
                    }
                    NameClass::AnyNameExcept(nc) => {
                        stack.push(nc.as_ref());
                        if !seen_anyname {
                            seen_anyname = true;
                            return Some((illegal_uri.clone(), illegal_local_name.clone()));
                        }
                    }
                    NameClass::NsName(ns) => {
                        if seen.insert((ns.clone(), illegal_local_name.clone())) {
                            return Some((ns.clone(), illegal_local_name.clone()));
                        }
                    }
                    NameClass::NsNameExcept(ns, nc) => {
                        stack.push(nc.as_ref());
                        if seen.insert((ns.clone(), illegal_local_name.clone())) {
                            return Some((ns.clone(), illegal_local_name.clone()));
                        }
                    }
                    NameClass::Name(ns, ln) => {
                        if seen.insert((ns.clone(), ln.clone())) {
                            return Some((ns.clone(), ln.clone()));
                        }
                    }
                    NameClass::NameClassChoice(nc1, nc2) => {
                        stack.push(nc1.as_ref());
                        stack.push(nc2.as_ref());
                    }
                }
            }
            None
        })
    }
}

#[derive(PartialEq, Eq, Hash)]
pub(super) enum Pattern {
    Empty,
    NotAllowed,
    Text,
    Choice(PatternId, PatternId),
    Interleave(PatternId, PatternId),
    Group(PatternId, PatternId),
    OneOrMore(PatternId),
    List(PatternId),
    Data(Datatype, ParamList),
    DataExcept(Datatype, ParamList, PatternId),
    Value(Datatype, Arc<str>, Context),
    Attribute(Arc<NameClass>, PatternId),
    Element(Arc<NameClass>, PatternId),
    After(PatternId, PatternId),
}

struct QName(Uri, LocalName);
struct AttributeNode(QName, Arc<str>);

enum ChildNode {
    ElementNode(QName, Context, Vec<AttributeNode>, Vec<ChildNode>),
    TextNode(Arc<str>),
}

pub(super) struct Grammar {
    pub(super) root: PatternId,
    pub(super) libraries: RelaxNGDatatypeLibraries,
    pub(super) patterns: Vec<Arc<Pattern>>,
    pub(super) intern: HashMap<Arc<Pattern>, PatternId>,
    /// -1: unknown, 0: false, 1: true
    pub(super) nullable: Vec<i8>,
}

impl Grammar {
    pub(super) fn new_validate_handler<H: SAXHandler>(
        &mut self,
        handler: H,
    ) -> ValidateHandler<'_, H> {
        let pattern = self.root;
        ValidateHandler {
            child: handler,
            grammar: self,
            pattern,
            locator: Arc::new(Locator::default()),
            base_uri: URIString::parse("").unwrap().into(),
            base_uri_stack: vec![],
            ns_stack: NamespaceStack::default(),
            text: String::new(),
            mixed: false,
            mixed_stack: vec![],
        }
    }

    pub(super) fn create_node(&mut self, pattern: Pattern) -> PatternId {
        if let Some(&index) = self.intern.get(&pattern) {
            index
        } else {
            let new = Arc::new(pattern);
            let at = self.patterns.len();
            self.intern.insert(new.clone(), at);
            self.patterns.push(new);
            self.nullable.push(-1);
            at
        }
    }

    fn nullable(&mut self, node: PatternId) -> bool {
        if self.nullable[node] >= 0 {
            return self.nullable[node] != 0;
        }
        let ret = match self.patterns[node].as_ref() {
            &Pattern::Group(p1, p2) => self.nullable(p1) && self.nullable(p2),
            &Pattern::Interleave(p1, p2) => self.nullable(p1) && self.nullable(p2),
            &Pattern::Choice(p1, p2) => self.nullable(p1) || self.nullable(p2),
            &Pattern::OneOrMore(p) => self.nullable(p),
            &Pattern::Element(_, _)
            | &Pattern::Attribute(_, _)
            | &Pattern::List(_)
            | &Pattern::Value(_, _, _)
            | &Pattern::Data(_, _)
            | &Pattern::DataExcept(_, _, _)
            | &Pattern::NotAllowed
            | &Pattern::After(_, _) => false,
            &Pattern::Empty | &Pattern::Text => true,
        };
        self.nullable[node] = ret as i8;
        ret
    }

    fn child_deriv(&mut self, cx: &Context, p: PatternId, child_node: &ChildNode) -> PatternId {
        match child_node {
            ChildNode::TextNode(s) => self.text_deriv(cx, p, s),
            ChildNode::ElementNode(qn, cx, atts, children) => {
                let p1 = self.start_tag_open_deriv(p, qn);
                let p2 = self.atts_deriv(cx, p1, atts);
                let p3 = self.start_tag_close_deriv(p2);
                let p4 = self.children_deriv(cx, p3, children);
                self.end_tag_deriv(p4)
            }
        }
    }

    fn text_deriv(&mut self, context: &Context, pattern: PatternId, string: &str) -> PatternId {
        match (context, self.patterns[pattern].as_ref(), string) {
            (cx, &Pattern::Choice(p1, p2), s) => {
                let p1 = self.text_deriv(cx, p1, s);
                let p2 = self.text_deriv(cx, p2, s);
                self.choice(p1, p2)
            }
            (cx, &Pattern::Interleave(p1, p2), s) => {
                let r1 = self.text_deriv(cx, p1, s);
                let q1 = self.interleave(r1, p2);
                let r2 = self.text_deriv(cx, p2, s);
                let q2 = self.interleave(p1, r2);
                self.choice(q1, q2)
            }
            (cx, &Pattern::Group(p1, p2), s) => {
                let q = self.text_deriv(cx, p1, s);
                let p = self.group(q, p2);
                if self.nullable(p1) {
                    let q = self.text_deriv(cx, p2, s);
                    self.choice(p, q)
                } else {
                    p
                }
            }
            (cx, &Pattern::After(p1, p2), s) => {
                let q = self.text_deriv(cx, p1, s);
                self.after(q, p2)
            }
            (cx, &Pattern::OneOrMore(p), s) => {
                let q1 = self.text_deriv(cx, p, s);
                let r1 = self.create_node(Pattern::OneOrMore(p));
                let r2 = self.create_node(Pattern::Empty);
                let q2 = self.choice(r1, r2);
                self.group(q1, q2)
            }
            (_, Pattern::Text, _) => pattern,
            (cx1, Pattern::Value(dt, value, cx2), s) => {
                if self.datatype_equal(dt, value, cx2, s, cx1) {
                    self.create_node(Pattern::Empty)
                } else {
                    self.create_node(Pattern::NotAllowed)
                }
            }
            (cx, Pattern::Data(dt, params), s) => {
                if self.datatype_allows(dt, params, s, cx) {
                    self.create_node(Pattern::Empty)
                } else {
                    self.create_node(Pattern::NotAllowed)
                }
            }
            (cx, &Pattern::DataExcept(ref dt, ref params, p), s) => {
                if self.datatype_allows(dt, params, s, cx)
                    && let q = self.text_deriv(cx, p, s)
                    && !self.nullable(q)
                {
                    self.create_node(Pattern::Empty)
                } else {
                    self.create_node(Pattern::NotAllowed)
                }
            }
            (cx, &Pattern::List(p), s) => {
                let q = self.list_deriv(
                    cx,
                    p,
                    &s.split(|c| XMLVersion::default().is_whitespace(c))
                        .collect::<Vec<_>>(),
                );
                if self.nullable(q) {
                    self.create_node(Pattern::Empty)
                } else {
                    self.create_node(Pattern::NotAllowed)
                }
            }
            (_, _, _) => self.create_node(Pattern::NotAllowed),
        }
    }

    fn list_deriv(&mut self, cx: &Context, p: PatternId, list: &[&str]) -> PatternId {
        match list {
            [] => p,
            [h, t @ ..] => {
                let p = self.text_deriv(cx, p, h);
                self.list_deriv(cx, p, t)
            }
        }
    }

    fn choice(&mut self, p1: PatternId, p2: PatternId) -> PatternId {
        if p1 == p2 {
            return p1;
        }
        match (self.patterns[p1].as_ref(), self.patterns[p2].as_ref()) {
            (_, Pattern::NotAllowed) => p1,
            (Pattern::NotAllowed, _) => p2,
            (_, _) => self.create_node(Pattern::Choice(p1.min(p2), p1.max(p2))),
        }
    }

    fn group(&mut self, p1: PatternId, p2: PatternId) -> PatternId {
        match (self.patterns[p1].as_ref(), self.patterns[p2].as_ref()) {
            (_, Pattern::NotAllowed) => p2,
            (Pattern::NotAllowed, _) => p1,
            (_, Pattern::Empty) => p1,
            (Pattern::Empty, _) => p2,
            (_, _) => self.create_node(Pattern::Group(p1, p2)),
        }
    }

    fn interleave(&mut self, p1: PatternId, p2: PatternId) -> PatternId {
        match (self.patterns[p1].as_ref(), self.patterns[p2].as_ref()) {
            (_, Pattern::NotAllowed) => p2,
            (Pattern::NotAllowed, _) => p1,
            (_, Pattern::Empty) => p1,
            (Pattern::Empty, _) => p2,
            (_, _) => self.create_node(Pattern::Interleave(p1.min(p2), p1.max(p2))),
        }
    }

    fn one_or_more(&mut self, p: PatternId) -> PatternId {
        match self.patterns[p].as_ref() {
            Pattern::NotAllowed => p,
            _ => self.create_node(Pattern::OneOrMore(p)),
        }
    }

    fn after(&mut self, p1: PatternId, p2: PatternId) -> PatternId {
        match (self.patterns[p1].as_ref(), self.patterns[p2].as_ref()) {
            (_, Pattern::NotAllowed) => p2,
            (Pattern::NotAllowed, _) => p1,
            (_, _) => self.create_node(Pattern::After(p1, p2)),
        }
    }

    fn datatype_allows(
        &self,
        dt: &Datatype,
        params: &ParamList,
        string: &str,
        context: &Context,
    ) -> bool {
        self.libraries
            .get(&dt.0)
            .and_then(|lib| lib.validate(&dt.1, params, string, context))
            .is_some_and(|b| b)
    }

    fn datatype_equal(
        &self,
        dt: &Datatype,
        s1: &str,
        cx1: &Context,
        s2: &str,
        cx2: &Context,
    ) -> bool {
        self.libraries
            .get(&dt.0)
            .and_then(|lib| lib.eq(&dt.1, s1, cx1, s2, cx2))
            .is_some_and(|b| b)
    }

    fn apply_after(
        &mut self,
        f: &impl Fn(&mut Grammar, PatternId) -> PatternId,
        pattern: PatternId,
    ) -> PatternId {
        match *self.patterns[pattern] {
            Pattern::After(p1, p2) => {
                let p2 = f(self, p2);
                self.after(p1, p2)
            }
            Pattern::Choice(p1, p2) => {
                let q1 = self.apply_after(f, p1);
                let q2 = self.apply_after(f, p2);
                self.choice(q1, q2)
            }
            Pattern::NotAllowed => pattern,
            _ => unreachable!(),
        }
    }

    fn start_tag_open_deriv(&mut self, pattern: PatternId, qn: &QName) -> PatternId {
        match *self.patterns[pattern] {
            Pattern::Choice(p1, p2) => {
                let q1 = self.start_tag_open_deriv(p1, qn);
                let q2 = self.start_tag_open_deriv(p2, qn);
                self.choice(q1, q2)
            }
            Pattern::Element(ref nc, p) => {
                if nc.contains(qn) {
                    let r = self.create_node(Pattern::Empty);
                    self.after(p, r)
                } else {
                    self.create_node(Pattern::NotAllowed)
                }
            }
            Pattern::Interleave(p1, p2) => {
                let r = self.start_tag_open_deriv(p1, qn);
                let q1 = self.apply_after(
                    &move |slf: &mut Grammar, p: PatternId| slf.interleave(p, p2),
                    r,
                );
                let r = self.start_tag_open_deriv(p2, qn);
                let q2 =
                    self.apply_after(&move |slf: &mut Grammar, p: usize| slf.interleave(p1, p), r);
                self.choice(q1, q2)
            }
            Pattern::OneOrMore(p) => {
                let q = self.start_tag_open_deriv(p, qn);
                self.apply_after(
                    &move |slf: &mut Grammar, p: usize| {
                        let r = slf.create_node(Pattern::Empty);
                        let q = slf.choice(pattern, r);
                        slf.group(p, q)
                    },
                    q,
                )
            }
            Pattern::Group(p1, p2) => {
                let q = self.start_tag_open_deriv(p1, qn);
                let x = self.apply_after(&move |slf: &mut Grammar, p1: usize| slf.group(p1, p2), q);
                if self.nullable(p1) {
                    let q = self.start_tag_open_deriv(p2, qn);
                    self.choice(x, q)
                } else {
                    x
                }
            }
            _ => self.create_node(Pattern::NotAllowed),
        }
    }

    fn atts_deriv(
        &mut self,
        cx: &Context,
        pattern: PatternId,
        atts: &[AttributeNode],
    ) -> PatternId {
        match atts {
            [] => pattern,
            [att, t @ ..] => {
                let pattern = self.att_deriv(cx, pattern, att);
                self.atts_deriv(cx, pattern, t)
            }
        }
    }

    fn att_deriv(&mut self, cx: &Context, pattern: PatternId, att: &AttributeNode) -> PatternId {
        match (self.patterns[pattern].as_ref(), att) {
            (&Pattern::After(p1, p2), att) => {
                let q = self.att_deriv(cx, p1, att);
                self.after(q, p2)
            }
            (&Pattern::Choice(p1, p2), att) => {
                let q1 = self.att_deriv(cx, p1, att);
                let q2 = self.att_deriv(cx, p2, att);
                self.choice(q1, q2)
            }
            (&Pattern::Group(p1, p2), att) => {
                let r1 = self.att_deriv(cx, p1, att);
                let q1 = self.group(r1, p2);
                let r2 = self.att_deriv(cx, p2, att);
                let q2 = self.group(p1, r2);
                self.choice(q1, q2)
            }
            (&Pattern::Interleave(p1, p2), att) => {
                let r1 = self.att_deriv(cx, p1, att);
                let q1 = self.interleave(r1, p2);
                let r2 = self.att_deriv(cx, p2, att);
                let q2 = self.interleave(p1, r2);
                self.choice(q1, q2)
            }
            (&Pattern::OneOrMore(p), att) => {
                let q1 = self.att_deriv(cx, p, att);
                let r = self.create_node(Pattern::Empty);
                let q2 = self.choice(pattern, r);
                self.group(q1, q2)
            }
            (Pattern::Attribute(nc, p), AttributeNode(qn, s)) => {
                if nc.contains(qn) && self.value_match(cx, *p, s) {
                    self.create_node(Pattern::Empty)
                } else {
                    self.create_node(Pattern::NotAllowed)
                }
            }
            (_, _) => self.create_node(Pattern::NotAllowed),
        }
    }

    fn value_match(&mut self, cx: &Context, p: PatternId, s: &str) -> bool {
        (self.nullable(p) && s.chars().all(|c| XMLVersion::default().is_whitespace(c))) || {
            let q = self.text_deriv(cx, p, s);
            self.nullable(q)
        }
    }

    fn start_tag_close_deriv(&mut self, pattern: PatternId) -> PatternId {
        match self.patterns[pattern].as_ref() {
            &Pattern::After(p1, p2) => {
                let p1 = self.start_tag_close_deriv(p1);
                self.after(p1, p2)
            }
            &Pattern::Choice(p1, p2) => {
                let q1 = self.start_tag_close_deriv(p1);
                let q2 = self.start_tag_close_deriv(p2);
                self.choice(q1, q2)
            }
            &Pattern::Group(p1, p2) => {
                let q1 = self.start_tag_close_deriv(p1);
                let q2 = self.start_tag_close_deriv(p2);
                self.group(q1, q2)
            }
            &Pattern::Interleave(p1, p2) => {
                let q1 = self.start_tag_close_deriv(p1);
                let q2 = self.start_tag_close_deriv(p2);
                self.interleave(q1, q2)
            }
            &Pattern::OneOrMore(p) => {
                let q = self.start_tag_close_deriv(p);
                self.one_or_more(q)
            }
            Pattern::Attribute(_, _) => self.create_node(Pattern::NotAllowed),
            _ => pattern,
        }
    }

    fn children_deriv(&mut self, cx: &Context, p: PatternId, children: &[ChildNode]) -> PatternId {
        match children {
            [] => self.children_deriv(cx, p, &[ChildNode::TextNode("".into())]),
            [ChildNode::TextNode(s)] => {
                let p1 = self.child_deriv(cx, p, &ChildNode::TextNode(s.clone()));
                if s.chars().all(|c| XMLVersion::default().is_whitespace(c)) {
                    self.choice(p, p1)
                } else {
                    p1
                }
            }
            children => self.strip_children_deriv(cx, p, children),
        }
    }

    fn strip_children_deriv(
        &mut self,
        cx: &Context,
        p: PatternId,
        children: &[ChildNode],
    ) -> PatternId {
        match children {
            [] => p,
            [h, t @ ..] => {
                let p = if self.strip(h) {
                    p
                } else {
                    self.child_deriv(cx, p, h)
                };
                self.strip_children_deriv(cx, p, t)
            }
        }
    }

    fn strip(&mut self, child: &ChildNode) -> bool {
        matches!(child, ChildNode::TextNode(s) if s.chars().all(|c| XMLVersion::default().is_whitespace(c)))
    }

    fn end_tag_deriv(&mut self, p: PatternId) -> PatternId {
        match *self.patterns[p].as_ref() {
            Pattern::Choice(p1, p2) => {
                let (p1, p2) = (self.end_tag_deriv(p1), self.end_tag_deriv(p2));
                self.choice(p1, p2)
            }
            Pattern::After(p1, p2) => {
                if self.nullable(p1) {
                    p2
                } else {
                    self.create_node(Pattern::NotAllowed)
                }
            }
            _ => self.create_node(Pattern::NotAllowed),
        }
    }
}

pub struct ValidateHandler<'a, H: SAXHandler> {
    pub child: H,
    grammar: &'a mut Grammar,

    // current pattern
    pattern: PatternId,

    // context
    locator: Arc<Locator>,
    base_uri: Arc<URIStr>,
    base_uri_stack: Vec<Arc<URIStr>>,
    ns_stack: NamespaceStack,

    // text buffer
    // `text` can be validated as chunked character content,
    // but `value` and `data` cannot be validated correctly
    // unless all character content is collected.
    text: String,

    // Indicates whether the current content is mixed content.
    // It is necessary for weak-match checking.
    mixed: bool,
    mixed_stack: Vec<bool>,
}

impl<'a, H: SAXHandler> ErrorHandler for ValidateHandler<'a, H> {
    fn fatal_error(&mut self, error: SAXParseError) {
        self.child.fatal_error(error);
    }

    fn error(&mut self, error: SAXParseError) {
        self.child.error(error);
    }

    fn warning(&mut self, error: SAXParseError) {
        self.child.warning(error);
    }
}
impl<'a, H: SAXHandler> EntityResolver for ValidateHandler<'a, H> {
    fn resolve_entity(
        &mut self,
        name: &str,
        public_id: Option<&str>,
        base_uri: &URIStr,
        system_id: &URIStr,
    ) -> Result<InputSource<'static>, XMLError> {
        self.child
            .resolve_entity(name, public_id, base_uri, system_id)
    }

    fn get_external_subset(
        &mut self,
        name: &str,
        base_uri: Option<&URIStr>,
    ) -> Result<InputSource<'static>, XMLError> {
        self.child.get_external_subset(name, base_uri)
    }
}
impl<'a, H: SAXHandler> SAXHandler for ValidateHandler<'a, H> {
    fn set_document_locator(&mut self, locator: Arc<Locator>) {
        self.child.set_document_locator(locator.clone());
        self.base_uri = locator.system_id();
        self.base_uri_stack.clear();
        self.ns_stack.clear();
        self.locator = locator;
        self.pattern = self.grammar.root;
        self.text.clear();
        self.mixed = false;
        self.mixed_stack.clear();
    }

    fn start_prefix_mapping(&mut self, prefix: Option<&str>, uri: &str) {
        self.ns_stack.push(prefix.unwrap_or_default(), uri);
        self.child.start_prefix_mapping(prefix, uri);
    }
    fn end_prefix_mapping(&mut self, prefix: Option<&str>) {
        // The popped prefix may not necessarily be `prefix`,
        // but the count matches up, so it's fine.
        self.ns_stack.pop();
        self.child.end_prefix_mapping(prefix);
    }

    fn start_element(
        &mut self,
        namespace_name: Option<&str>,
        local_name: Option<&str>,
        qname: &str,
        atts: &Attributes,
    ) {
        if !self.text.is_empty() {
            if !self
                .text
                .chars()
                .all(|c| XMLVersion::default().is_whitespace(c))
            {
                let ns_map = self
                    .ns_stack
                    .iter()
                    .map(|ns| (ns.prefix.clone(), ns.namespace_name.clone()))
                    .collect::<BTreeMap<_, _>>();
                self.pattern = self.grammar.text_deriv(
                    &(self.base_uri.clone(), ns_map),
                    self.pattern,
                    &self.text,
                );
            }
            self.text.clear();
        }
        let mut base_uri = self.base_uri.clone();
        let mut attributes = vec![];
        for att in atts {
            if !att.is_nsdecl() {
                let namespace_name = att.namespace_name.clone().unwrap_or_default();
                let local_name = att.local_name.clone().unwrap_or_default();
                let value = att.value.as_ref();
                // `xml:base`
                if att.qname.as_ref() == "xml:base"
                    && let Ok(uri) = URIString::parse(value)
                {
                    base_uri = base_uri.resolve(&uri).into();
                }
                attributes.push(AttributeNode(
                    QName(namespace_name, local_name),
                    value.into(),
                ));
            }
        }

        self.base_uri_stack.push(base_uri.clone());
        self.base_uri = base_uri.clone();
        self.mixed_stack.push(true);
        self.mixed = false;

        let ns_map = self
            .ns_stack
            .iter()
            .map(|ns| (ns.prefix.clone(), ns.namespace_name.clone()))
            .collect::<BTreeMap<_, _>>();
        let p1 = self.grammar.start_tag_open_deriv(
            self.pattern,
            &QName(
                namespace_name.unwrap_or_default().into(),
                local_name.unwrap_or_default().into(),
            ),
        );
        let p2 = self
            .grammar
            .atts_deriv(&(base_uri, ns_map), p1, &attributes);
        let p3 = self.grammar.start_tag_close_deriv(p2);
        self.pattern = p3;

        self.child
            .start_element(namespace_name, local_name, qname, atts);
    }
    fn end_element(&mut self, namespace_name: Option<&str>, local_name: Option<&str>, qname: &str) {
        if !self.text.is_empty() || !self.mixed {
            let ns_map = self
                .ns_stack
                .iter()
                .map(|ns| (ns.prefix.clone(), ns.namespace_name.clone()))
                .collect::<BTreeMap<_, _>>();
            let p =
                self.grammar
                    .text_deriv(&(self.base_uri.clone(), ns_map), self.pattern, &self.text);
            if self
                .text
                .chars()
                .all(|c| XMLVersion::default().is_whitespace(c))
            {
                self.pattern = self.grammar.choice(self.pattern, p);
            } else {
                self.pattern = p;
            }
            self.text.clear();
        }

        self.pattern = self.grammar.end_tag_deriv(self.pattern);
        if let Some(base_uri) = self.base_uri_stack.pop() {
            self.base_uri = base_uri;
        }
        if let Some(mixed) = self.mixed_stack.pop() {
            self.mixed = mixed;
        }
        self.child.end_element(namespace_name, local_name, qname);
    }

    fn characters(&mut self, data: &str) {
        self.text.push_str(data);
        self.child.characters(data);
    }

    // Following callbacks are not used for RELAX NG validation.
    // They simply forward received events to the child handler.

    fn attribute_decl(
        &mut self,
        element_name: &str,
        attribute_name: &str,
        attribute_type: &AttributeType,
        default_decl: &DefaultDecl,
    ) {
        self.child
            .attribute_decl(element_name, attribute_name, attribute_type, default_decl);
    }
    fn comment(&mut self, data: &str) {
        self.child.comment(data);
    }
    fn declaration(&mut self, version: &str, encoding: Option<&str>, standalone: Option<bool>) {
        self.child.declaration(version, encoding, standalone);
    }
    fn element_decl(&mut self, name: &str, contentspec: &ContentSpec) {
        self.child.element_decl(name, contentspec);
    }
    fn external_entity_decl(&mut self, name: &str, public_id: Option<&str>, system_id: &URIStr) {
        self.child.external_entity_decl(name, public_id, system_id);
    }
    fn ignorable_whitespace(&mut self, data: &str) {
        self.child.ignorable_whitespace(data);
    }
    fn internal_entity_decl(&mut self, name: &str, value: &str) {
        self.child.internal_entity_decl(name, value);
    }
    fn notation_decl(&mut self, name: &str, public_id: Option<&str>, system_id: Option<&URIStr>) {
        self.child.notation_decl(name, public_id, system_id);
    }
    fn processing_instruction(&mut self, target: &str, data: Option<&str>) {
        self.child.processing_instruction(target, data);
    }
    fn skipped_entity(&mut self, name: &str) {
        self.child.skipped_entity(name);
    }
    fn start_cdata(&mut self) {
        self.child.start_cdata();
    }
    fn end_cdata(&mut self) {
        self.child.end_cdata();
    }
    fn start_document(&mut self) {
        self.child.start_document();
    }
    fn end_document(&mut self) {
        self.child.end_document();
    }
    fn start_dtd(&mut self, name: &str, public_id: Option<&str>, system_id: Option<&URIStr>) {
        self.child.start_dtd(name, public_id, system_id);
    }
    fn end_dtd(&mut self) {
        self.child.end_dtd();
    }
    fn start_entity(&mut self, name: &str) {
        self.child.start_entity(name);
    }
    fn end_entity(&mut self) {
        self.child.end_entity();
    }
    fn unparsed_entity_decl(
        &mut self,
        name: &str,
        public_id: Option<&str>,
        system_id: &URIStr,
        notation_name: &str,
    ) {
        self.child
            .unparsed_entity_decl(name, public_id, system_id, notation_name);
    }
}
