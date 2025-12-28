use std::io::Read;

use anyxml_uri::uri::URIString;

use crate::{
    DefaultParserSpec,
    error::XMLError,
    sax::{
        handler::{DefaultSAXHandler, SAXHandler},
        parser::XMLReader,
    },
    tree::{Document, Element, Node, TreeBuildHandler, node::NodeSpec},
    uri::URIStr,
};

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

pub enum XIncludeError {}

pub struct XIncludeProcessor<
    'a,
    H: SAXHandler = DefaultSAXHandler,
    R: XIncludeResourceResolver = XIncludeDefaultResourceResolver,
> {
    pub reader: XMLReader<DefaultParserSpec<'a>, TreeBuildHandler<H>>,
    pub resolver: Option<R>,
}

impl<H: SAXHandler, R: XIncludeResourceResolver> XIncludeProcessor<'_, H, R> {
    pub fn process(&mut self, document: Document) -> Result<Document, XIncludeError> {
        todo!()
    }

    pub fn process_once(&mut self, element: Element) -> Result<Element, XIncludeError> {
        todo!()
    }

    pub fn process_subtree(
        &mut self,
        root: Node<dyn NodeSpec>,
    ) -> Result<Node<dyn NodeSpec>, XIncludeError> {
        todo!()
    }
}
