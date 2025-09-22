pub mod events;
mod handler;

use std::{io::Read, marker::PhantomData, sync::Arc};

use anyxml_uri::uri::URIStr;

use crate::{
    ProgressiveParserSpec,
    error::XMLError,
    sax::{
        NamespaceStack,
        error::SAXParseError,
        handler::{DefaultSAXHandler, EntityResolver, ErrorHandler},
        parser::{
            ParserConfig, ParserOption, XMLProgressiveReaderBuilder, XMLReader, XMLReaderBuilder,
        },
        source::INPUT_CHUNK,
    },
    stax::{
        events::{
            Declaration, EndElement, ProcessingInstruction, StartElement, XMLEvent, XMLEventType,
        },
        handler::XMLStreamReaderHandler,
    },
};

pub struct XMLStreamReader<
    'a,
    Resolver: EntityResolver = DefaultSAXHandler,
    Reporter: ErrorHandler = DefaultSAXHandler,
> {
    source: Box<dyn Read + 'a>,
    buffer: Vec<u8>,

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
    pub fn parse_uri(
        &mut self,
        uri: impl AsRef<URIStr>,
        encoding: Option<&str>,
    ) -> Result<(), XMLError> {
        self.reader.reset()?;
        if let Some(encoding) = encoding {
            self.reader.set_encoding(encoding);
        }
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

    pub fn parse_reader(
        &mut self,
        source: impl Read + 'a,
        encoding: Option<&str>,
        uri: Option<&URIStr>,
    ) -> Result<(), XMLError> {
        self.reader.reset()?;
        if let Some(uri) = uri {
            let mut base_uri = self.reader.base_uri.resolve(uri);
            base_uri.normalize();
            self.reader.base_uri = base_uri.into();
        }
        if let Some(encoding) = encoding {
            self.reader.set_encoding(encoding);
        }
        self.source = Box::new(source);
        self.buffer.clear();
        self.buffer.resize(INPUT_CHUNK, 0);
        Ok(())
    }

    pub fn parse_str(&mut self, s: &str, uri: Option<&URIStr>) -> Result<(), XMLError> {
        self.reader.reset()?;
        if let Some(uri) = uri {
            let mut base_uri = self.reader.base_uri.resolve(uri);
            base_uri.normalize();
            self.reader.base_uri = base_uri.into();
        }

        self.source = Box::new(std::io::empty());
        self.buffer.clear();
        self.buffer.extend(s.as_bytes());
        Ok(())
    }

    pub fn reset(&mut self) -> Result<(), XMLError> {
        self.source = Box::new(std::io::empty());
        self.buffer.clear();
        self.reader.reset()
    }

    pub fn next_event<'b>(&'b mut self) -> Result<XMLEvent<'b>, XMLError> {
        if self.reader.handler.event == XMLEventType::Finished {
            return Ok(XMLEvent::Finished);
        } else if self.reader.handler.event == XMLEventType::EndEmptyTag {
            return Ok(self.create_event());
        }

        while !self.reader.parse_event_once(false)? || self.reader.handler.in_dtd {
            let len = self.source.read(&mut self.buffer)?;
            if len == 0 {
                // this should report `XMLError::ParserUnexpectedEOF` or `EndDocument` event.
                self.reader.parse_event_once(true)?;
                break;
            }

            self.reader.source.push_bytes(&self.buffer[..len], false)?;
        }
        Ok(self.create_event())
    }

    fn create_event<'b>(&'b mut self) -> XMLEvent<'b> {
        self.reader.handler.reported = true;
        match self.reader.handler.event {
            XMLEventType::StartDocument => XMLEvent::StartDocument,
            XMLEventType::EndDocument => {
                self.reader.handler.event = XMLEventType::Finished;
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
            XMLEventType::Finished => XMLEvent::Finished,
        }
    }

    pub fn next_tag<'b>(&'b mut self) -> Result<XMLEvent<'b>, XMLError> {
        if self.reader.handler.event == XMLEventType::Finished {
            return Ok(XMLEvent::Finished);
        } else if self.reader.handler.event == XMLEventType::EndEmptyTag {
            return Ok(self.create_event());
        }

        while !self.reader.parse_event_once(false)?
            || self.reader.handler.in_dtd
            || !matches!(
                self.reader.handler.event,
                XMLEventType::StartElement
                    | XMLEventType::EndElement
                    | XMLEventType::StartEmptyTag
                    | XMLEventType::EndEmptyTag
            )
        {
            let len = self.source.read(&mut self.buffer)?;
            if len == 0 {
                // this should report `XMLError::ParserUnexpectedEOF` or `EndDocument` event.
                self.reader.parse_event_once(true)?;
                break;
            }

            self.reader.source.push_bytes(&self.buffer[..len], false)?;
        }
        Ok(self.create_event())
    }

    pub fn namespaces(&self) -> &NamespaceStack {
        &self.reader.namespaces
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
            reader: self.builder.set_handler(handler).build(),
        }
    }
}

impl Default for XMLStreamReaderBuilder<'_> {
    fn default() -> Self {
        Self::new()
    }
}
