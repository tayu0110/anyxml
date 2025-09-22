use anyxml_uri::uri::{URIStr, URIString};

use crate::{
    sax::{
        attributes::Attributes,
        handler::{DefaultSAXHandler, EntityResolver, ErrorHandler, SAXHandler},
    },
    stax::events::XMLEventType,
};

pub(crate) struct XMLStreamReaderHandler<Resolver: EntityResolver = DefaultSAXHandler> {
    pub(super) event: XMLEventType,
    pub(super) namespace_name: Option<String>,
    pub(super) local_name: Option<String>,
    pub(super) qname: String,
    pub(super) atts: Attributes,
    pub(super) system_id: Option<URIString>,
    pub(super) in_cdsect: bool,
    pub(super) in_dtd: bool,
    pub(super) reported: bool,

    pub(super) entity_resolver: Option<Resolver>,
}

impl<Resolver: EntityResolver> Default for XMLStreamReaderHandler<Resolver> {
    fn default() -> Self {
        Self {
            event: Default::default(),
            namespace_name: Default::default(),
            local_name: Default::default(),
            qname: Default::default(),
            atts: Default::default(),
            system_id: Default::default(),
            in_cdsect: Default::default(),
            in_dtd: Default::default(),
            reported: Default::default(),
            entity_resolver: None,
        }
    }
}

impl<Resolver: EntityResolver> EntityResolver for XMLStreamReaderHandler<Resolver> {
    fn get_external_subset(
        &mut self,
        name: &str,
        base_uri: Option<&URIStr>,
    ) -> Result<crate::sax::source::InputSource<'static>, crate::error::XMLError> {
        if let Some(entity_resolver) = self.entity_resolver.as_mut() {
            entity_resolver.get_external_subset(name, base_uri)
        } else {
            DefaultSAXHandler.get_external_subset(name, base_uri)
        }
    }

    fn resolve_entity(
        &mut self,
        name: &str,
        public_id: Option<&str>,
        base_uri: &URIStr,
        system_id: &URIStr,
    ) -> Result<crate::sax::source::InputSource<'static>, crate::error::XMLError> {
        if let Some(entity_resolver) = self.entity_resolver.as_mut() {
            entity_resolver.resolve_entity(name, public_id, base_uri, system_id)
        } else {
            DefaultSAXHandler.resolve_entity(name, public_id, base_uri, system_id)
        }
    }
}

impl<Resolver: EntityResolver> ErrorHandler for XMLStreamReaderHandler<Resolver> {}

impl<Resolver: EntityResolver> SAXHandler for XMLStreamReaderHandler<Resolver> {
    fn start_document(&mut self) {
        self.event = XMLEventType::StartDocument;
        self.reported = false;
    }

    fn end_document(&mut self) {
        self.event = XMLEventType::EndDocument;
        self.reported = false;
    }

    fn start_element(
        &mut self,
        uri: Option<&str>,
        local_name: Option<&str>,
        qname: &str,
        atts: &Attributes,
    ) {
        self.event = XMLEventType::StartElement;
        if let Some(uri) = uri {
            let buf = self.namespace_name.get_or_insert_default();
            buf.clear();
            buf.push_str(uri);
        } else {
            self.namespace_name = None;
        }
        if let Some(local_name) = local_name {
            let buf = self.local_name.get_or_insert_default();
            buf.clear();
            buf.push_str(local_name);
        } else {
            self.local_name = None;
        }
        self.qname.clear();
        self.qname.push_str(qname);
        self.atts = atts.clone();
        self.reported = false;
    }

    fn end_element(&mut self, uri: Option<&str>, local_name: Option<&str>, qname: &str) {
        if matches!(self.event, XMLEventType::StartElement) && !self.reported {
            self.event = XMLEventType::StartEmptyTag;
            return;
        }
        self.event = XMLEventType::EndElement;
        if let Some(uri) = uri {
            let buf = self.namespace_name.get_or_insert_default();
            buf.clear();
            buf.push_str(uri);
        } else {
            self.namespace_name = None;
        }
        if let Some(local_name) = local_name {
            let buf = self.local_name.get_or_insert_default();
            buf.clear();
            buf.push_str(local_name);
        } else {
            self.local_name = None;
        }
        self.qname.clear();
        self.qname.push_str(qname);
        self.reported = false;
    }

    fn declaration(&mut self, _version: &str, _encoding: Option<&str>, _standalone: Option<bool>) {
        self.event = XMLEventType::Declaration;
        self.reported = false;
    }

    fn start_dtd(&mut self, name: &str, public_id: Option<&str>, system_id: Option<&URIStr>) {
        self.event = XMLEventType::DocumentType;
        self.qname.clear();
        self.qname.push_str(name);
        if let Some(public_id) = public_id {
            let buf = self.local_name.get_or_insert_default();
            buf.clear();
            buf.push_str(public_id);
        } else {
            self.local_name = None;
        }
        self.system_id = system_id.map(|uri| uri.to_owned());
        self.in_dtd = true;
        self.reported = false;
    }

    fn end_dtd(&mut self) {
        self.in_dtd = false;
        self.reported = false;
    }

    fn characters(&mut self, data: &str) {
        if !matches!(self.event, XMLEventType::Characters | XMLEventType::Space) && !self.in_cdsect
        {
            self.qname.clear();
            self.event = XMLEventType::Characters;
            self.reported = false;
        }
        self.qname.push_str(data);
    }

    fn ignorable_whitespace(&mut self, data: &str) {
        if !matches!(self.event, XMLEventType::Characters | XMLEventType::Space) && !self.in_cdsect
        {
            self.qname.clear();
            self.event = XMLEventType::Space;
            self.reported = false;
        }
        self.qname.push_str(data);
    }

    fn start_cdata(&mut self) {
        self.event = XMLEventType::CDATASection;
        self.qname.clear();
        self.in_cdsect = true;
        self.reported = false;
    }

    fn end_cdata(&mut self) {
        self.in_cdsect = false;
        self.reported = false;
    }

    fn comment(&mut self, data: &str) {
        if self.in_dtd {
            return;
        }
        if !matches!(self.event, XMLEventType::Comment) {
            self.qname.clear();
        }
        self.event = XMLEventType::Comment;
        self.qname.push_str(data);
        self.reported = false;
    }

    fn processing_instruction(&mut self, target: &str, data: Option<&str>) {
        if self.in_dtd {
            return;
        }
        self.event = XMLEventType::ProcessingInstruction;
        self.qname.clear();
        self.qname.push_str(target);
        if let Some(data) = data {
            let buf = self.local_name.get_or_insert_default();
            buf.clear();
            buf.push_str(data);
        } else {
            self.local_name = None;
        }
        self.reported = false;
    }

    fn start_entity(&mut self, name: &str) {
        if self.in_dtd {
            return;
        }
        self.event = XMLEventType::StartEntity;
        self.qname.clear();
        self.qname.push_str(name);
        self.reported = false;
    }

    fn end_entity(&mut self) {
        if self.in_dtd {
            return;
        }
        self.event = XMLEventType::EndEntity;
        self.reported = false;
    }
}
