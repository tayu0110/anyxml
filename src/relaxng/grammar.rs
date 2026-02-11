use std::collections::{BTreeMap, HashSet};

use crate::{
    error::XMLError,
    relaxng::{
        datatype_library::RelaxNGDatatypeLibraries,
        parse::{RelaxNGNodeType, RelaxNGParseHandler},
    },
    sax::handler::SAXHandler,
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
