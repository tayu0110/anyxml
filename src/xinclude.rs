use std::io::Read;

use crate::{
    DefaultParserSpec,
    error::XMLError,
    sax::{
        handler::{DefaultSAXHandler, SAXHandler},
        parser::XMLReader,
    },
    tree::{Document, Element, Node, TreeBuildHandler, node::NodeSpec},
    uri::{URIStr, URIString},
};

pub const XML_XINCLUDE_NAMESPACE: &str = "http://www.w3.org/2001/XInclude";

pub trait XIncludeResourceResolver {
    fn resolve_xml(
        &mut self,
        href: &URIStr,
        accept: Option<&str>,
        accept_language: Option<&str>,
    ) -> Result<XIncludeResource, XMLError> {
        let _ = (accept, accept_language);
        Err(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            format!(
                "The XML resource '{}' is not found.",
                href.as_unescaped_str()
                    .as_deref()
                    .unwrap_or(href.as_escaped_str())
            ),
        )
        .into())
    }
    fn resolve_text(
        &mut self,
        href: &URIStr,
        accept: Option<&str>,
        accept_language: Option<&str>,
    ) -> Result<XIncludeResource, XMLError> {
        let _ = (accept, accept_language);
        Err(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            format!(
                "The text resource '{}' is not found.",
                href.as_unescaped_str()
                    .as_deref()
                    .unwrap_or(href.as_escaped_str())
            ),
        )
        .into())
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
    UnknownError,
}

pub struct XIncludeProcessor<
    'a,
    H: SAXHandler = DefaultSAXHandler,
    R: XIncludeResourceResolver = XIncludeDefaultResourceResolver,
> {
    pub reader: XMLReader<DefaultParserSpec<'a>, TreeBuildHandler<H>>,
    pub resolver: Option<R>,
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
        // let mut fallback = None;
        let mut children = include.first_child();
        while let Some(child) = children {
            children = child.next_sibling();
        }
        todo!()
    }
}
