pub mod events;
mod handler;

use std::io::Read;

use anyxml_uri::uri::URIStr;

use crate::{
    ProgressiveParserSpec,
    error::XMLError,
    sax::{
        NamespaceStack,
        handler::EntityResolver,
        parser::{XMLReader, XMLReaderBuilder},
        source::INPUT_CHUNK,
    },
    stax::{
        events::{
            Declaration, EndElement, ProcessingInstruction, StartElement, XMLEvent, XMLEventType,
        },
        handler::XMLStreamReaderHandler,
    },
};

pub struct XMLStreamReader<'a> {
    source: Box<dyn Read + 'a>,
    buffer: Vec<u8>,

    reader: XMLReader<ProgressiveParserSpec, XMLStreamReaderHandler>,
}

impl<'a> XMLStreamReader<'a> {
    pub fn parse_uri(uri: impl AsRef<URIStr>, encoding: Option<&str>) -> Result<Self, XMLError> {
        let mut reader = XMLReaderBuilder::new()
            .set_handler(XMLStreamReaderHandler::default())
            .progressive_parser()
            .build();
        if let Some(encoding) = encoding {
            reader.set_encoding(encoding);
        }
        let source = Box::new(reader.handler.resolve_entity(
            "[document]",
            None,
            &reader.base_uri,
            uri.as_ref(),
        )?);
        let (source, buffer) = source.decompose();
        if !buffer.is_empty() {
            reader.source.push_bytes(buffer, false)?;
        }
        Ok(Self {
            source,
            buffer: vec![0; INPUT_CHUNK],
            reader,
        })
    }

    pub fn parse_reader(
        source: impl Read + 'a,
        encoding: Option<&str>,
        uri: Option<&URIStr>,
    ) -> Result<Self, XMLError> {
        let mut reader = XMLReaderBuilder::new()
            .set_handler(XMLStreamReaderHandler::default())
            .progressive_parser();
        if let Some(uri) = uri {
            reader = reader.set_default_base_uri(uri)?;
        }
        let mut reader = reader.build();
        if let Some(encoding) = encoding {
            reader.set_encoding(encoding);
        }
        Ok(Self {
            source: Box::new(source),
            buffer: vec![0; INPUT_CHUNK],
            reader,
        })
    }

    pub fn parse_str(s: &str, uri: Option<&URIStr>) -> Result<Self, XMLError> {
        let mut reader = XMLReaderBuilder::new()
            .set_handler(XMLStreamReaderHandler::default())
            .progressive_parser();
        if let Some(uri) = uri {
            reader = reader.set_default_base_uri(uri)?;
        }
        Ok(Self {
            source: Box::new(std::io::empty()),
            buffer: s.as_bytes().to_vec(),
            reader: reader.build(),
        })
    }

    pub fn next_event<'b>(&'b mut self) -> Result<XMLEvent<'b>, XMLError> {
        if self.reader.handler.event == XMLEventType::Finished {
            return Ok(XMLEvent::Finished);
        }

        while !self.reader.parse_event_once(false)? {
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
            XMLEventType::EndElement => XMLEvent::EndElement(EndElement {
                namespace_name: self.reader.handler.namespace_name.as_deref(),
                local_name: self.reader.handler.local_name.as_deref(),
                qname: &self.reader.handler.qname,
            }),
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
            XMLEventType::EntityReference => XMLEvent::EntityReference(&self.reader.handler.qname),
            XMLEventType::Finished => XMLEvent::Finished,
        }
    }

    pub fn next_tag<'b>(&'b mut self) -> Result<XMLEvent<'b>, XMLError> {
        loop {
            let event = self.next_event()?;
            if matches!(
                event,
                XMLEvent::StartElement(_) | XMLEvent::EndElement(_) | XMLEvent::Finished
            ) {
                break;
            }
        }
        Ok(self.create_event())
    }

    pub fn namespaces(&self) -> &NamespaceStack {
        &self.reader.namespaces
    }
}
