use std::io::Read;

use crate::{
    DefaultParserSpec,
    error::XMLError,
    sax::{
        handler::{DefaultSAXHandler, SAXHandler},
        parser::XMLReader,
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
impl XIncludeResourceResolver for XIncludeDefaultResourceResolver {}

pub struct XIncludeResource {
    pub resource: Box<dyn Read>,
    pub base_uri: URIString,
    pub media_type: Option<String>,
    pub encoding: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XIncludeError {
    BaseURINotFound,
    UnknownError,
}

pub struct XIncludeProcessor<
    'a,
    H: SAXHandler = DefaultSAXHandler,
    R: XIncludeResourceResolver = XIncludeDefaultResourceResolver,
> {
    pub reader: XMLReader<DefaultParserSpec<'a>, TreeBuildHandler<H>>,
    pub resolver: R,
}

impl<H: SAXHandler, R: XIncludeResourceResolver> XIncludeProcessor<'_, H, R> {
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
            if let Some(mut include) = child.as_element().filter(|elem| {
                elem.local_name().as_ref() == "include"
                    && elem.namespace_name().as_deref() == Some(XML_XINCLUDE_NAMESPACE)
            }) {
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

                let expanded =
                    self.expand_include_element(root.owner_document(), include.clone())?;
                include.replace_subtree(expanded)?;
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
                        todo!("fatal error");
                    }
                    fallback = Some(child.clone());
                } else {
                    todo!("fatal error")
                }
            }
        }

        let parse = include
            .get_attribute("parse", None)
            .unwrap_or_else(|| "xml".to_owned());

        let ret = match parse.as_str() {
            "xml" => self.expand_xml(source_document, include)?,
            "text" => self.expand_text(source_document, include)?,
            _ => todo!("fatal error"),
        };

        if let Some(ret) = ret {
            Ok(ret)
        } else {
            todo!("fallback");
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
        while !source.content_str().is_empty() {
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
            // same document reference
            let Some(xpointer) = include.get_attribute("xpointer", None) else {
                todo!("fatal error");
            };

            let xpointer = parse_xpointer(&xpointer)?;
            return Ok(xpointer.resolve(source_document.clone()));
        }

        let href = include
            .base_uri()
            .ok_or(XIncludeError::BaseURINotFound)?
            .resolve(&URIString::parse(href)?);
        let accept = include.get_attribute("accept", None);
        let accept_language = include.get_attribute("accept-language", None);

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
            Ok(()) => Ok(Some(self.reader.handler.document.clone().into())),
            Err(err) => {
                if matches!(media_type.as_deref(), Some("text/xml" | "application/xml")) {
                    // If resource is not XML media type, it is treated as resource error,
                    Ok(None)
                } else {
                    // otherwise, it is fatal error because of non-well-formed XML document.
                    Err(err)
                }
            }
        }
    }
}
