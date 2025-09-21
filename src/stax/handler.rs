use anyxml_uri::uri::{URIStr, URIString};

use crate::{
    sax::{
        attributes::Attributes,
        handler::{EntityResolver, SAXHandler},
    },
    stax::events::XMLEventType,
};

#[derive(Default)]
pub(crate) struct XMLStreamReaderHandler {
    pub(super) event: XMLEventType,
    pub(super) namespace_name: Option<String>,
    pub(super) local_name: Option<String>,
    pub(super) qname: String,
    pub(super) atts: Attributes,
    pub(super) system_id: Option<URIString>,
    pub(super) in_cdsect: bool,
}

impl EntityResolver for XMLStreamReaderHandler {}

impl SAXHandler for XMLStreamReaderHandler {
    fn start_document(&mut self) {
        self.event = XMLEventType::StartDocument;
    }

    fn end_document(&mut self) {
        self.event = XMLEventType::EndDocument;
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
    }

    fn end_element(&mut self, uri: Option<&str>, local_name: Option<&str>, qname: &str) {
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
    }

    fn declaration(&mut self, _version: &str, _encoding: Option<&str>, _standalone: Option<bool>) {
        self.event = XMLEventType::Declaration;
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
    }

    fn characters(&mut self, data: &str) {
        if !matches!(self.event, XMLEventType::Characters | XMLEventType::Space) && !self.in_cdsect
        {
            self.qname.clear();
        }
        self.event = XMLEventType::Characters;
        self.qname.push_str(data);
    }

    fn ignorable_whitespace(&mut self, data: &str) {
        if !matches!(self.event, XMLEventType::Characters | XMLEventType::Space) && !self.in_cdsect
        {
            self.qname.clear();
        }
        self.event = XMLEventType::Space;
        self.qname.push_str(data);
    }

    fn start_cdata(&mut self) {
        self.event = XMLEventType::CDATASection;
        self.qname.clear();
        self.in_cdsect = true;
    }

    fn end_cdata(&mut self) {
        self.in_cdsect = false;
    }

    fn comment(&mut self, data: &str) {
        if !matches!(self.event, XMLEventType::Comment) {
            self.qname.clear();
        }
        self.event = XMLEventType::Comment;
        self.qname.push_str(data);
    }

    fn processing_instruction(&mut self, target: &str, data: Option<&str>) {
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
    }

    fn start_entity(&mut self, name: &str) {
        self.event = XMLEventType::EntityReference;
        self.qname.clear();
        self.qname.push_str(name);
    }
}
