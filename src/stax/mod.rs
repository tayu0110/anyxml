//! Provide StAX style XML parser and event types.

pub mod events;
mod handler;

use std::{io::Read, marker::PhantomData, sync::Arc};

use crate::{
    ProgressiveParserSpec,
    error::XMLError,
    sax::{
        Locator, NamespaceStack,
        error::{SAXParseError, fatal_error},
        handler::{DefaultSAXHandler, EntityResolver, ErrorHandler},
        parser::{
            ParserConfig, ParserOption, ParserState, XMLProgressiveReaderBuilder, XMLReader,
            XMLReaderBuilder,
        },
        source::INPUT_CHUNK,
    },
    stax::{
        events::{
            Declaration, EndElement, ProcessingInstruction, StartElement, XMLEvent, XMLEventType,
        },
        handler::XMLStreamReaderHandler,
    },
    uri::URIStr,
};

pub struct XMLStreamReader<
    'a,
    Resolver: EntityResolver = DefaultSAXHandler,
    Reporter: ErrorHandler = DefaultSAXHandler,
> {
    source: Box<dyn Read + 'a>,
    buffer: Vec<u8>,
    eof: bool,

    reader: XMLReader<ProgressiveParserSpec, XMLStreamReaderHandler<Resolver, Reporter>>,
}

impl<'a, Resolver: EntityResolver> XMLStreamReader<'a, Resolver> {
    /// Returns the most recently occurred warning, error, or fatal error.  \
    /// If none occurred, returns `None`.
    ///
    /// When no user-defined error handler is configured, this method can be used to catch errors
    /// other than unrecoverable errors.  \
    /// To catch all errors that occur during the parsing process, a user-defined error handler
    /// must be configured.
    pub fn last_error(&self) -> Option<&SAXParseError> {
        self.reader.handler.last_error.as_ref()
    }
}

impl<'a, Resolver: EntityResolver, Reporter: ErrorHandler> XMLStreamReader<'a, Resolver, Reporter> {
    /// Retrieves and parses the XML document specified by `uri`.  \
    /// If retrieval or parsing of the XML document fails, an error is returned.
    ///
    /// The preferred encoding can be specified using `encoding`.
    pub fn parse_uri(
        &mut self,
        uri: impl AsRef<URIStr>,
        encoding: Option<&str>,
    ) -> Result<(), XMLError> {
        self.reset()?;
        if let Some(encoding) = encoding {
            self.reader.set_encoding(encoding);
        }
        let mut base_uri = self.reader.default_base_uri()?.resolve(uri.as_ref());
        base_uri.normalize();
        self.reader.base_uri = base_uri.into();
        self.reader.locator = Arc::new(Locator::new(self.reader.base_uri.clone(), None, 1, 1));
        let source = Box::new(self.reader.handler.resolve_entity(
            "[document]",
            None,
            &self.reader.base_uri,
            uri.as_ref(),
        )?);
        let (source, buffer) = source.decompose();
        if !buffer.is_empty() {
            self.reader.source.push_bytes(buffer, false)?;
        }
        self.source = source;
        self.buffer.clear();
        self.buffer.resize(INPUT_CHUNK, 0);
        Ok(())
    }

    /// The data read from `reader` is parsed as an XML document.  \
    /// If parsing of the XML document fails, an error is returned.
    ///
    /// The preferred encoding can be specified using `encoding`.
    ///
    /// `uri` is treated as the document's base URI. It is optional to set,
    /// but may be required if the document being parsed references external resources.
    pub fn parse_reader(
        &mut self,
        reader: impl Read + 'a,
        encoding: Option<&str>,
        uri: Option<&URIStr>,
    ) -> Result<(), XMLError> {
        self.reset()?;
        if let Some(uri) = uri {
            let mut base_uri = self.reader.default_base_uri()?.resolve(uri);
            base_uri.normalize();
            self.reader.base_uri = base_uri.into();
            self.reader.locator = Arc::new(Locator::new(self.reader.base_uri.clone(), None, 1, 1));
        }
        if let Some(encoding) = encoding {
            self.reader.set_encoding(encoding);
        }
        self.source = Box::new(reader);
        self.buffer.clear();
        self.buffer.resize(INPUT_CHUNK, 0);
        Ok(())
    }

    /// Parses `xml` as an XML document.  \
    /// If parsing of the XML document fails, an error is returned.
    ///
    /// Assumes the document is encoded in UTF-8.
    ///
    /// `uri` is treated as the document's base URI. It is optional to set,
    /// but may be required if the document being parsed references external resources.
    pub fn parse_str(&mut self, xml: &str, uri: Option<&URIStr>) -> Result<(), XMLError> {
        self.reset()?;
        if let Some(uri) = uri {
            let mut base_uri = self.reader.default_base_uri()?.resolve(uri);
            base_uri.normalize();
            self.reader.base_uri = base_uri.into();
            self.reader.locator = Arc::new(Locator::new(self.reader.base_uri.clone(), None, 1, 1));
        }

        self.source = Box::new(std::io::empty());
        self.buffer.clear();
        self.reader.source.push_bytes(xml.as_bytes(), true)?;
        Ok(())
    }

    pub fn reset(&mut self) -> Result<(), XMLError> {
        self.source = Box::new(std::io::empty());
        self.buffer.clear();
        self.eof = false;
        self.reader.reset()?;
        self.reader.handler.reset();
        Ok(())
    }

    pub fn next_event<'b>(&'b mut self) -> Result<XMLEvent<'b>, XMLError> {
        (|| {
            if self.reader.handler.event == XMLEventType::Finished {
                while self.reader.parse_event_once(self.eof)? {}
                return Ok(());
            } else if self.reader.handler.event == XMLEventType::EndEmptyTag {
                return Ok(());
            }
            while !self.reader.parse_event_once(self.eof)? || self.reader.handler.in_dtd {
                if self.eof {
                    if self.reader.state != ParserState::Finished {
                        return Err(XMLError::ParserUnexpectedEOF);
                    } else if self.reader.handler.event == XMLEventType::FatalError {
                        self.reader.handler.event = XMLEventType::Finished;
                    }
                    break;
                } else {
                    let len = self.source.read(&mut self.buffer)?;
                    if len == 0 {
                        self.eof = true;
                    } else {
                        self.reader.source.push_bytes(&self.buffer[..len], false)?;
                    }
                }
            }
            Ok(())
        })()
        .inspect_err(|err| {
            fatal_error!(self.reader, err, "Unrecoverable error: {}", err);
        })?;
        Ok(self.create_event())
    }

    fn create_event<'b>(&'b mut self) -> XMLEvent<'b> {
        self.reader.handler.reported = true;
        match self.reader.handler.event {
            XMLEventType::StartDocument => XMLEvent::StartDocument,
            XMLEventType::EndDocument => {
                self.reader.handler.event = XMLEventType::Finished;
                self.reader.handler.locator = None;
                XMLEvent::EndDocument
            }
            XMLEventType::StartElement => XMLEvent::StartElement(StartElement {
                namespace_name: self.reader.handler.namespace_name.as_deref(),
                local_name: self.reader.handler.local_name.as_deref(),
                qname: &self.reader.handler.qname,
                atts: &self.reader.handler.atts,
            }),
            XMLEventType::EndElement | XMLEventType::EndEmptyTag => {
                self.reader.handler.event = XMLEventType::EndElement;
                XMLEvent::EndElement(EndElement {
                    namespace_name: self.reader.handler.namespace_name.as_deref(),
                    local_name: self.reader.handler.local_name.as_deref(),
                    qname: &self.reader.handler.qname,
                })
            }
            XMLEventType::StartEmptyTag => {
                self.reader.handler.event = XMLEventType::EndEmptyTag;
                self.reader.handler.reported = false;
                XMLEvent::StartElement(StartElement {
                    namespace_name: self.reader.handler.namespace_name.as_deref(),
                    local_name: self.reader.handler.local_name.as_deref(),
                    qname: &self.reader.handler.qname,
                    atts: &self.reader.handler.atts,
                })
            }
            XMLEventType::Declaration => XMLEvent::Declaration(Declaration {
                version: self.reader.version,
                encoding: self.reader.encoding.as_deref(),
                standalone: self.reader.standalone,
            }),
            XMLEventType::DocumentType => XMLEvent::DocumentType,
            XMLEventType::Characters => XMLEvent::Characters(&self.reader.handler.qname),
            XMLEventType::CDATASection => XMLEvent::CDATASection(&self.reader.handler.qname),
            XMLEventType::Space => XMLEvent::Space(&self.reader.handler.qname),
            XMLEventType::Comment => XMLEvent::Comment(&self.reader.handler.qname),
            XMLEventType::ProcessingInstruction => {
                XMLEvent::ProcessingInstruction(ProcessingInstruction {
                    target: &self.reader.handler.qname,
                    data: self.reader.handler.local_name.as_deref(),
                })
            }
            XMLEventType::StartEntity => XMLEvent::StartEntity(&self.reader.handler.qname),
            XMLEventType::EndEntity => XMLEvent::EndEntity,
            XMLEventType::FatalError => XMLEvent::FatalError,
            XMLEventType::Finished => XMLEvent::Finished,
        }
    }

    pub fn next_tag<'b>(&'b mut self) -> Result<XMLEvent<'b>, XMLError> {
        (|| {
            if self.reader.handler.event == XMLEventType::Finished {
                while self.reader.parse_event_once(self.eof)? {}
                return Ok(());
            } else if self.reader.handler.event == XMLEventType::EndEmptyTag {
                return Ok(());
            }

            while !self.reader.parse_event_once(self.eof)?
                || self.reader.handler.in_dtd
                || !matches!(
                    self.reader.handler.event,
                    XMLEventType::StartElement
                        | XMLEventType::EndElement
                        | XMLEventType::StartEmptyTag
                        | XMLEventType::EndEmptyTag
                )
            {
                if self.eof {
                    if self.reader.state != ParserState::Finished {
                        return Err(XMLError::ParserUnexpectedEOF);
                    } else if self.reader.handler.event == XMLEventType::FatalError {
                        self.reader.handler.event = XMLEventType::Finished;
                    }
                    break;
                } else {
                    let len = self.source.read(&mut self.buffer)?;
                    if len == 0 {
                        self.eof = true;
                    } else {
                        self.reader.source.push_bytes(&self.buffer[..len], false)?;
                    }
                }
            }
            Ok(())
        })()
        .inspect_err(|err| {
            fatal_error!(self.reader, err, "Unrecoverable error: {}", err);
        })?;

        Ok(self.create_event())
    }

    pub fn namespaces(&self) -> &NamespaceStack {
        &self.reader.namespaces
    }

    /// Returns a valid locator if exists.  \
    /// The returned locator is valid from after the `StartDocument` event until
    /// before parsing ends due to the `EndDocument` event or a fatal error.  \
    /// Outside the locator's valid range, it returns `None`.
    pub fn locator(&self) -> Option<Arc<Locator>> {
        self.reader.handler.locator.clone()
    }

    /// If a user-defined error handler is configured, it will be returned.
    pub fn error_handler(&self) -> Option<&Reporter> {
        self.reader.handler.error_handler.as_ref()
    }

    /// Configure a user-defined error handler. If a handler is already configured, return it.
    pub fn set_error_handler(&mut self, handler: Reporter) -> Option<Reporter> {
        self.reader.handler.error_handler.replace(handler)
    }

    /// If a user-defined error handler is configured, it will be returned.  \
    /// The error handler will be unset.
    pub fn take_error_handler(&mut self) -> Option<Reporter> {
        self.reader.handler.error_handler.take()
    }
}

impl Default for XMLStreamReader<'_> {
    fn default() -> Self {
        XMLStreamReaderBuilder::new().build()
    }
}

pub struct XMLStreamReaderBuilder<
    'a,
    Resolver: EntityResolver = DefaultSAXHandler,
    Reporter: ErrorHandler = DefaultSAXHandler,
> {
    builder: XMLProgressiveReaderBuilder<XMLStreamReaderHandler>,
    entity_resolver: Option<Resolver>,
    error_handler: Option<Reporter>,
    _phantom: PhantomData<&'a ()>,
}

impl<'a> XMLStreamReaderBuilder<'a> {
    pub fn new() -> Self {
        Self {
            builder: XMLReaderBuilder::new()
                .set_handler(XMLStreamReaderHandler::default())
                .progressive_parser(),
            entity_resolver: None,
            error_handler: None,
            _phantom: PhantomData,
        }
    }
}

impl<'a, Resolver: EntityResolver, Reporter: ErrorHandler>
    XMLStreamReaderBuilder<'a, Resolver, Reporter>
{
    pub fn set_default_base_uri(self, base_uri: impl Into<Arc<URIStr>>) -> Result<Self, XMLError> {
        Ok(Self {
            builder: self.builder.set_default_base_uri(base_uri)?,
            entity_resolver: self.entity_resolver,
            error_handler: self.error_handler,
            _phantom: PhantomData,
        })
    }

    pub fn set_entity_resolver<Other: EntityResolver>(
        self,
        resolver: Other,
    ) -> XMLStreamReaderBuilder<'a, Other, Reporter> {
        XMLStreamReaderBuilder {
            builder: self.builder,
            entity_resolver: Some(resolver),
            error_handler: self.error_handler,
            _phantom: PhantomData,
        }
    }

    pub fn set_error_handler<Other: ErrorHandler>(
        self,
        error_handler: Other,
    ) -> XMLStreamReaderBuilder<'a, Resolver, Other> {
        XMLStreamReaderBuilder {
            builder: self.builder,
            entity_resolver: self.entity_resolver,
            error_handler: Some(error_handler),
            _phantom: PhantomData,
        }
    }

    pub fn set_parser_config(self, config: ParserConfig) -> Self {
        Self {
            builder: self.builder.set_parser_config(config),
            entity_resolver: self.entity_resolver,
            error_handler: self.error_handler,
            _phantom: PhantomData,
        }
    }
    pub fn enable_option(self, option: ParserOption) -> Self {
        Self {
            builder: self.builder.enable_option(option),
            entity_resolver: self.entity_resolver,
            error_handler: self.error_handler,
            _phantom: PhantomData,
        }
    }
    pub fn disable_option(self, option: ParserOption) -> Self {
        Self {
            builder: self.builder.disable_option(option),
            entity_resolver: self.entity_resolver,
            error_handler: self.error_handler,
            _phantom: PhantomData,
        }
    }

    pub fn build(self) -> XMLStreamReader<'a, Resolver, Reporter> {
        let handler = XMLStreamReaderHandler {
            entity_resolver: self.entity_resolver,
            error_handler: self.error_handler,
            ..Default::default()
        };
        XMLStreamReader {
            source: Box::new(std::io::empty()),
            buffer: vec![],
            eof: false,
            reader: self.builder.set_handler(handler).build(),
        }
    }
}

impl Default for XMLStreamReaderBuilder<'_> {
    fn default() -> Self {
        Self::new()
    }
}
