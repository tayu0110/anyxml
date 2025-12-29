//! XInclude processing APIs and data types.
//!
//!
//!
//! # Reference
//! - [XML Inclusions (XInclude) Version 1.0 (Second Edition)](https://www.w3.org/TR/2006/REC-xinclude-20061115/)

use std::{collections::HashMap, fs::File, io::Read};

use crate::{
    DefaultParserSpec, XML_XML_NAMESPACE,
    error::XMLError,
    sax::{
        handler::{DefaultSAXHandler, SAXHandler},
        parser::{XMLReader, XMLReaderBuilder},
        source::InputSource,
    },
    tree::{Document, Element, Node, TreeBuildHandler, node::NodeSpec},
    uri::{URIStr, URIString},
    xpointer::parse_xpointer,
};

pub const XML_XINCLUDE_NAMESPACE: &str = "http://www.w3.org/2001/XInclude";

pub trait XIncludeResourceResolver {
    fn resolve_xml(
        &mut self,
        href: &URIStr,
        accept: Option<&str>,
        accept_language: Option<&str>,
    ) -> Option<XIncludeResource> {
        let _ = (href, accept, accept_language);
        None
    }
    fn resolve_text(
        &mut self,
        href: &URIStr,
        accept: Option<&str>,
        accept_language: Option<&str>,
    ) -> Option<XIncludeResource> {
        let _ = (href, accept, accept_language);
        None
    }
}

pub struct XIncludeDefaultResourceResolver;
impl XIncludeResourceResolver for XIncludeDefaultResourceResolver {
    fn resolve_xml(
        &mut self,
        href: &URIStr,
        _accept: Option<&str>,
        _accept_language: Option<&str>,
    ) -> Option<XIncludeResource> {
        let file = File::open(href.path()).ok()?;
        Some(XIncludeResource {
            resource: Box::new(file),
            base_uri: href.to_owned(),
            media_type: None,
            encoding: None,
        })
    }

    fn resolve_text(
        &mut self,
        href: &URIStr,
        accept: Option<&str>,
        accept_language: Option<&str>,
    ) -> Option<XIncludeResource> {
        self.resolve_xml(href, accept, accept_language)
    }
}

pub struct XIncludeResource {
    pub resource: Box<dyn Read>,
    pub base_uri: URIString,
    pub media_type: Option<String>,
    pub encoding: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XIncludeError {
    UnacceptableElement,
    UnacceptableAttributeValue,
    InclusionLoop,
    BaseURINotFound,
    ResourceResolutionFailed,
    NonWellFormedInclusionTarget,
    UnknownError,
}

macro_rules! fatal_error {
    ( $proc:ident, $document:expr, $code:expr, $msg:literal, $( $arg:expr ),* ) => {{
        use self::XIncludeError::*;
        #[allow(unused)]
        use $crate::error::XMLError::*;
        use crate::sax::handler::ErrorHandler;
        $proc.reader.handler.fatal_error($crate::sax::error::SAXParseError {
            error: $code.into(),
            level: $crate::error::XMLErrorLevel::FatalError,
            domain: $crate::error::XMLErrorDomain::XInclude,
            line: 0,
            column: 0,
            system_id: $document.document_base_uri().as_ref().into(),
            public_id: None,
            message: ::std::borrow::Cow::Owned(format!($msg, $( $arg ),*)),
        });
    }};
    ( $proc:ident, $document:expr, $code:expr, $msg:literal ) => {
        fatal_error!($proc, $document, $code, $msg, );
    };
}

pub struct XIncludeProcessor<
    'a,
    H: SAXHandler = DefaultSAXHandler,
    R: XIncludeResourceResolver = XIncludeDefaultResourceResolver,
> {
    pub reader: XMLReader<DefaultParserSpec<'a>, TreeBuildHandler<H>>,
    pub resolver: R,
    document_cache: HashMap<(URIString, Option<String>, Option<String>), Document>,
    include_stack: Vec<Element>,
}

impl<H: SAXHandler, R: XIncludeResourceResolver> XIncludeProcessor<'_, H, R> {
    pub fn new<'a>(
        reader: XMLReader<DefaultParserSpec<'a>, TreeBuildHandler<H>>,
        resolver: R,
    ) -> XIncludeProcessor<'a, H, R> {
        XIncludeProcessor {
            reader,
            resolver,
            document_cache: HashMap::new(),
            include_stack: vec![],
        }
    }

    pub fn process(&mut self, document: Document) -> Result<Document, XMLError> {
        let ret = self.process_subtree(document.into())?;
        Ok(ret.as_document().ok_or(XIncludeError::UnknownError)?)
    }

    pub fn process_subtree(
        &mut self,
        root: Node<dyn NodeSpec>,
    ) -> Result<Node<dyn NodeSpec>, XMLError> {
        let ret = root.deep_copy_subtree()?;
        let mut children = Some(ret.clone());
        while let Some(child) = children {
            if let Some(mut include) = child
                .as_element()
                .filter(|elem| elem.namespace_name().as_deref() == Some(XML_XINCLUDE_NAMESPACE))
            {
                if include.local_name().as_ref() != "include" {
                    fatal_error!(
                        self,
                        include.owner_document(),
                        UnacceptableElement,
                        "The element '{}' in XInclude namespace is unacceptable at this position.",
                        include.local_name()
                    );
                    return Err(XIncludeError::UnacceptableElement.into());
                }
                if self
                    .include_stack
                    .iter()
                    .any(|elem| include.is_same_node(elem))
                {
                    fatal_error!(
                        self,
                        include.owner_document(),
                        InclusionLoop,
                        "Includsion loop is detected."
                    );
                    return Err(XIncludeError::InclusionLoop.into());
                }
                self.include_stack.push(include.clone());

                if let Some(next) = child.next_sibling() {
                    children = Some(next);
                } else {
                    children = None;

                    let mut parent = child.parent_node();
                    while let Some(par) = parent {
                        if let Some(next) = par.next_sibling() {
                            children = Some(next);
                            break;
                        }
                        parent = par.parent_node();
                    }
                }

                let mut expanded =
                    self.expand_include_element(root.owner_document(), include.clone())?;
                if let Some(document) = expanded.as_document() {
                    expanded = document
                        .document_element()
                        .ok_or(XIncludeError::UnknownError)?
                        .into();
                }
                let mut expanded = include.owner_document().import_node(expanded);
                fixup_base_uri(
                    include.parent_node().map(From::from),
                    include.clone(),
                    &mut expanded,
                )?;
                include.replace_subtree(expanded)?;
                self.include_stack.pop();
            } else if let Some(first) = child.first_child() {
                children = Some(first);
            } else if let Some(next) = child.next_sibling() {
                children = Some(next);
            } else {
                children = None;

                let mut parent = child.parent_node();
                while let Some(par) = parent {
                    if let Some(next) = par.next_sibling() {
                        children = Some(next);
                        break;
                    }
                    parent = par.parent_node();
                }
            }
        }

        Ok(ret)
    }

    fn expand_include_element(
        &mut self,
        source_document: Document,
        include: Element,
    ) -> Result<Node<dyn NodeSpec>, XMLError> {
        let mut fallback = None;
        let mut children = include.first_element_child();
        while let Some(child) = children {
            children = child.next_element_sibling();

            if child.namespace_name().as_deref() == Some(XML_XINCLUDE_NAMESPACE) {
                if child.local_name().as_ref() == "fallback" {
                    if fallback.is_some() {
                        fatal_error!(
                            self,
                            include.owner_document(),
                            UnacceptableElement,
                            "Multiple fallbacks are unacceptable as children of 'include'.",
                        );
                        return Err(XIncludeError::UnacceptableElement.into());
                    }
                    fallback = Some(child.clone());
                } else {
                    fatal_error!(
                        self,
                        include.owner_document(),
                        UnacceptableElement,
                        "The element '{}' in XInclude namespace is unacceptable as a child of 'include'.",
                        include.local_name()
                    );
                    return Err(XIncludeError::UnacceptableElement.into());
                }
            }
        }

        let parse = include
            .get_attribute("parse", None)
            .unwrap_or_else(|| "xml".to_owned());

        let ret = match parse.as_str() {
            "xml" => self.expand_xml(source_document, include.clone())?,
            "text" => self.expand_text(source_document, include.clone())?,
            value => {
                fatal_error!(
                    self,
                    include.owner_document(),
                    UnacceptableAttributeValue,
                    "'{}' is unacceptable value as 'parse' in 'include'.",
                    value
                );
                return Err(XIncludeError::UnacceptableAttributeValue.into());
            }
        };

        if let Some(ret) = ret {
            Ok(ret)
        } else if let Some(fallback) = fallback {
            let mut fragment = include.owner_document().create_document_fragment();
            let mut children = fallback.first_child();
            while let Some(child) = children {
                children = child.next_sibling();

                fragment.append_child(self.process_subtree(child)?)?;
            }
            Ok(fragment.into())
        } else {
            fatal_error!(
                self,
                include.owner_document(),
                ResourceResolutionFailed,
                "The resource resolution for 'include' is failed: href='{}', parse='{}', xpointer='{}', encoding='{}'",
                include.get_attribute("href", None).unwrap_or_default(),
                include.get_attribute("parse", None).unwrap_or_default(),
                include.get_attribute("xpointer", None).unwrap_or_default(),
                include.get_attribute("encoding", None).unwrap_or_default()
            );
            Err(XIncludeError::ResourceResolutionFailed.into())
        }
    }

    fn expand_text(
        &mut self,
        source_document: Document,
        include: Element,
    ) -> Result<Option<Node<dyn NodeSpec>>, XMLError> {
        let href = include.get_attribute("href", None).unwrap_or_default();
        let accept = include.get_attribute("accept", None);
        let accept_language = include.get_attribute("accept-language", None);
        self.validate_accept_attribute(
            include.owner_document(),
            accept.as_deref(),
            accept_language.as_deref(),
        )?;
        let encoding = include.get_attribute("encoding", None);

        let href = if href.is_empty() {
            source_document.document_base_uri().as_ref().to_owned()
        } else {
            let href = URIString::parse(href)?;
            include
                .base_uri()
                .ok_or(XIncludeError::BaseURINotFound)?
                .resolve(&href)
        };

        let Some(mut resource) =
            self.resolver
                .resolve_text(&href, accept.as_deref(), accept_language.as_deref())
        else {
            // resource error
            return Ok(None);
        };

        // decide encoding method following "4.3 Included Items when parse="text"" in XInclude spec.
        let mut source = if let Some(encoding) = resource.encoding.as_deref() {
            InputSource::from_reader(&mut resource.resource, Some(encoding))?
        } else if matches!(
            resource.media_type.as_deref(),
            Some("text/xml" | "application/xml")
        ) {
            // TODO: Add a feature to the library that correctly validates XML media types.
            InputSource::from_reader(&mut resource.resource, None)?
        } else if encoding.is_some() {
            InputSource::from_reader(&mut resource.resource, encoding.as_deref())?
        } else {
            InputSource::from_reader(&mut resource.resource, Some("UTF-8"))?
        };

        // decode and construct resource string
        source.set_compact_mode();
        source.grow()?;
        let mut buf = String::new();
        while !source.is_empty() {
            let s = source.content_str();
            let len = s.len();
            buf.push_str(s);
            source.advance(len)?;
            source.grow()?;
        }

        let text = include.owner_document().create_text(buf);
        Ok(Some(text.into()))
    }

    fn expand_xml(
        &mut self,
        source_document: Document,
        include: Element,
    ) -> Result<Option<Node<dyn NodeSpec>>, XMLError> {
        let href = include.get_attribute("href", None).unwrap_or_default();

        if href.is_empty() {
            // self document reference
            let Some(xpointer) = include.get_attribute("xpointer", None) else {
                fatal_error!(
                    self,
                    include.owner_document(),
                    InclusionLoop,
                    "'xpointer' attribute must be set for 'include' specified href=''."
                );
                return Err(XIncludeError::InclusionLoop.into());
            };

            let xpointer = parse_xpointer(&xpointer)?;
            let resource = xpointer.resolve(source_document.clone());
            return if let Some(resource) = resource {
                Ok(Some(self.process_subtree(resource)?))
            } else {
                Ok(None)
            };
        }

        let href = include
            .base_uri()
            .ok_or(XIncludeError::BaseURINotFound)?
            .resolve(&URIString::parse(href)?);
        let accept = include.get_attribute("accept", None);
        let accept_language = include.get_attribute("accept-language", None);
        self.validate_accept_attribute(
            include.owner_document(),
            accept.as_deref(),
            accept_language.as_deref(),
        )?;

        if let Some(document) =
            self.document_cache
                .get(&(href.clone(), accept.clone(), accept_language.clone()))
        {
            if let Some(xpointer) = include.get_attribute("xpointer", None) {
                let xpointer = parse_xpointer(&xpointer)?;
                return if let Some(resource) = xpointer.resolve(document.clone()) {
                    Ok(Some(self.process_subtree(resource)?))
                } else {
                    Ok(None)
                };
            } else {
                // reference loop
                fatal_error!(
                    self,
                    include.owner_document(),
                    InclusionLoop,
                    "Includsion loop is detected."
                );
                return Err(XIncludeError::InclusionLoop.into());
            }
        }

        let Some(XIncludeResource {
            resource,
            base_uri,
            media_type,
            encoding,
        }) = self
            .resolver
            .resolve_xml(&href, accept.as_deref(), accept_language.as_deref())
        else {
            // resource error
            return Ok(None);
        };

        match self
            .reader
            .parse_reader(resource, encoding.as_deref(), Some(&base_uri))
        {
            Ok(()) => {
                let document = self.reader.handler.document.clone();
                self.document_cache
                    .insert((href, accept, accept_language), document.clone());
                let mut ret = if let Some(xpointer) = include.get_attribute("xpointer", None) {
                    let xpointer = parse_xpointer(&xpointer)?;
                    xpointer.resolve(document)
                } else {
                    Some(document.into())
                };

                if let Some(tree) = ret {
                    ret = Some(self.process_subtree(tree)?);
                }

                Ok(ret)
            }
            Err(err) => {
                if matches!(media_type.as_deref(), Some("text/xml" | "application/xml")) {
                    // If resource is not XML media type, it is treated as resource error,
                    Ok(None)
                } else {
                    // otherwise, it is fatal error because of non-well-formed XML document.
                    fatal_error!(
                        self,
                        include.owner_document(),
                        NonWellFormedInclusionTarget,
                        "The resource '{}' is non-well-formed XML document because of '{}'.",
                        href.as_unescaped_str()
                            .as_deref()
                            .unwrap_or(href.as_escaped_str()),
                        err
                    );
                    Err(err)
                }
            }
        }
    }

    fn validate_accept_attribute(
        &mut self,
        document: Document,
        accept: Option<&str>,
        accept_language: Option<&str>,
    ) -> Result<(), XIncludeError> {
        let mut ret = Ok(());
        if let Some(accept) = accept
            && accept.bytes().any(|b| !matches!(b, 0x20..=0x7E))
        {
            fatal_error!(
                self,
                document,
                UnacceptableAttributeValue,
                "'{}' is invalid for 'accept' value of 'include'.",
                accept
            );
            ret = Err(XIncludeError::UnacceptableAttributeValue);
        }

        if let Some(accept_language) = accept_language
            && accept_language.bytes().any(|b| !matches!(b, 0x20..=0x7E))
        {
            fatal_error!(
                self,
                document,
                UnacceptableAttributeValue,
                "'{}' is invalid for 'accept-language' value of 'include'.",
                accept_language
            );
            ret = Err(XIncludeError::UnacceptableAttributeValue);
        }

        ret
    }
}

impl Default for XIncludeProcessor<'_> {
    fn default() -> Self {
        Self {
            reader: XMLReaderBuilder::new()
                .set_handler(TreeBuildHandler::default())
                .build(),
            resolver: XIncludeDefaultResourceResolver,
            document_cache: HashMap::new(),
            include_stack: vec![],
        }
    }
}

fn fixup_base_uri(
    include_parent: Option<Node<dyn NodeSpec>>,
    include: Element,
    node: &mut Node<dyn NodeSpec>,
) -> Result<(), XMLError> {
    let pb = include_parent.and_then(|p| p.base_uri());
    let nb = node.base_uri();

    if pb != nb
        && let Some(mut element) = node.as_element()
        && let Some(href) = include.get_attribute("href", None)
    {
        element.set_attribute("xml:base", Some(XML_XML_NAMESPACE), Some(&href))?;
    }
    Ok(())
}
