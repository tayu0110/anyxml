//! StAX style XML writer.

use std::{borrow::Cow, io::Write};

use anyxml_encoding::find_encoder;

use crate::{
    encoding::{Encoder, UTF8Encoder},
    error::XMLError,
    sax::{
        DefaultSAXHandler, INPUT_CHUNK, ParserConfig, ProgressiveParserSpec, SAXHandler, XMLReader,
    },
    uri::URIString,
};

struct OutputStream<'a> {
    stream: Box<dyn Write + 'a>,
    buffer: Vec<u8>,
    encoder: Box<dyn Encoder>,
}

impl OutputStream<'_> {
    fn write(&mut self, mut data: &str) -> Result<(), XMLError> {
        while !data.is_empty() {
            let (read, write) = self.encoder.encode(data, &mut self.buffer, false)?;
            data = &data[read..];
            self.stream.write_all(&self.buffer[..write])?;
        }
        Ok(())
    }

    fn flush(&mut self) -> Result<(), XMLError> {
        self.stream.flush()?;
        Ok(())
    }
}

impl Default for OutputStream<'_> {
    fn default() -> Self {
        Self {
            stream: Box::new(vec![]),
            buffer: vec![0; INPUT_CHUNK],
            encoder: Box::new(UTF8Encoder),
        }
    }
}

enum State {
    BeforeStart,
    StartDTD,
    InDTD,
    StartElement,
    InContent,
    Finished,
}

pub struct XMLStreamWriter<'a, H: SAXHandler = DefaultSAXHandler> {
    stream: OutputStream<'a>,
    checker: XMLReader<ProgressiveParserSpec, H>,
    // QName
    name_stack: Vec<String>,
    // (namespace, prefix, depth)
    ns_stack: Vec<(String, String, usize)>,
    state: State,
    /// If the default namespace is bound and an element with a namespace of NULL is
    /// written as an unqualified name, the meaning changes. Set this flag to `true`
    /// when it is necessary to remove the default namespace to prevent this.
    should_insert_default_namespace: bool,
}

macro_rules! write_tokens {
    ( $self:expr, $( $e:expr ),* ) => {
        $(
            $self.checker.parse_chunk($e.as_bytes(), false)?;
        )*
        $(
            $self.stream.write($e)?;
        )*
    };
}

impl<'a> XMLStreamWriter<'a> {
    pub fn new(writer: impl Write + 'a, config: Option<ParserConfig>) -> Self {
        let stream = OutputStream {
            stream: Box::new(writer),
            ..Default::default()
        };

        Self {
            stream,
            checker: XMLReader::builder()
                .progressive_parser()
                .set_parser_config(config.unwrap_or_default())
                .build(),
            name_stack: vec![],
            ns_stack: vec![],
            state: State::BeforeStart,
            should_insert_default_namespace: false,
        }
    }
}

impl<'a, H: SAXHandler> XMLStreamWriter<'a, H> {
    pub fn with_handler(writer: impl Write + 'a, handler: H, config: Option<ParserConfig>) -> Self {
        let stream = OutputStream {
            stream: Box::new(writer),
            ..Default::default()
        };

        Self {
            stream,
            checker: XMLReader::builder()
                .progressive_parser()
                .set_handler(handler)
                .set_parser_config(config.unwrap_or_default())
                .build(),
            name_stack: vec![],
            ns_stack: vec![],
            state: State::BeforeStart,
            should_insert_default_namespace: false,
        }
    }

    pub fn set_base_uri(&mut self, base_uri: impl Into<URIString>) -> Result<(), XMLError> {
        self.checker.set_default_base_uri(base_uri.into())
    }

    pub fn write_declaration(
        &mut self,
        version: &str,
        encoding: Option<&str>,
        standalone: Option<bool>,
    ) -> Result<(), XMLError> {
        let encoding = if let Some(encoding) = encoding {
            self.stream.encoder =
                find_encoder(encoding).ok_or(XMLError::ParserUnsupportedEncoding)?;
            Cow::Owned(format!(" encoding=\"{encoding}\""))
        } else {
            Cow::Borrowed("")
        };
        let standalone =
            standalone.map(|s| format!(" standalone=\"{}\"", if s { "yes" } else { "no" }));
        let standalone = standalone.as_deref().unwrap_or_default();
        write_tokens!(
            self,
            "<?xml version=\"",
            version,
            "\"",
            encoding.as_ref(),
            standalone,
            "?>"
        );
        Ok(())
    }

    pub fn write_start_dtd(
        &mut self,
        name: &str,
        public_id: Option<&str>,
        system_id: Option<&str>,
    ) -> Result<(), XMLError> {
        let system_id = system_id.map(|s| {
            format!(
                " {}\"{s}\"",
                if public_id.is_some() { "" } else { "SYSTEM " }
            )
        });
        let system_id = system_id.as_deref().unwrap_or_default();
        let public_id = public_id.map(|p| format!(" PUBLIC \"{p}\""));
        let public_id = public_id.as_deref().unwrap_or_default();
        write_tokens!(self, "<!DOCTYPE ", name, public_id, system_id);
        self.state = State::StartDTD;
        Ok(())
    }
    pub fn write_end_dtd(&mut self) -> Result<(), XMLError> {
        if matches!(self.state, State::StartDTD) {
            write_tokens!(self, ">");
        } else {
            write_tokens!(self, "]>");
        }
        Ok(())
    }
    pub fn write_element_decl(&mut self, name: &str, contentspec: &str) -> Result<(), XMLError> {
        self.try_start_internal_subset()?;
        write_tokens!(self, "<!ELEMENT ", name, " ", contentspec, ">");
        Ok(())
    }
    pub fn write_attribute_decl(
        &mut self,
        element_name: &str,
        attribute_name: &str,
        attribute_type: &str,
        default_decl: &str,
    ) -> Result<(), XMLError> {
        self.try_start_internal_subset()?;
        write_tokens!(
            self,
            "<!ATTLIST ",
            element_name,
            " ",
            attribute_name,
            " ",
            attribute_type,
            " ",
            default_decl,
            ">"
        );
        Ok(())
    }
    pub fn write_internal_entity_decl(&mut self, name: &str, value: &str) -> Result<(), XMLError> {
        self.try_start_internal_subset()?;
        write_tokens!(self, "<!ENTITY ", name, " \"", value, "\">");
        Ok(())
    }
    pub fn write_external_entity_decl(
        &mut self,
        name: &str,
        public_id: Option<&str>,
        system_id: &str,
    ) -> Result<(), XMLError> {
        self.try_start_internal_subset()?;
        let system_id = format!(
            " {}\"{system_id}\"",
            if public_id.is_none() { "" } else { "SYSTEM " }
        );
        let public_id = public_id.map(|p| format!(" PUBLIC \"{p}\""));
        let public_id = public_id.as_deref().unwrap();
        write_tokens!(self, "<!ENTITY ", name, public_id, &system_id, ">");
        Ok(())
    }
    pub fn write_unparsed_entity_decl(
        &mut self,
        name: &str,
        public_id: Option<&str>,
        system_id: &str,
        notation_name: &str,
    ) -> Result<(), XMLError> {
        self.try_start_internal_subset()?;
        let system_id = format!(
            " {}\"{system_id}\"",
            if public_id.is_none() { "" } else { "SYSTEM " }
        );
        let public_id = public_id.map(|p| format!(" PUBLIC \"{p}\""));
        let public_id = public_id.as_deref().unwrap();
        write_tokens!(
            self,
            "<!ENTITY ",
            name,
            public_id,
            &system_id,
            " NDATA ",
            notation_name,
            ">"
        );
        Ok(())
    }
    pub fn write_notation_decl(
        &mut self,
        name: &str,
        public_id: Option<&str>,
        system_id: Option<&str>,
    ) -> Result<(), XMLError> {
        self.try_start_internal_subset()?;
        let system_id = system_id.map(|system_id| {
            format!(
                " {}\"{system_id}\"",
                if public_id.is_none() { "" } else { "SYSTEM " }
            )
        });
        let system_id = system_id.as_deref().unwrap_or_default();
        let public_id = public_id.map(|p| format!(" PUBLIC \"{p}\""));
        let public_id = public_id.as_deref().unwrap();
        write_tokens!(self, "<!NOTATION ", name, public_id, &system_id, ">");
        Ok(())
    }

    pub fn write_start_element(&mut self, qname: &str) -> Result<(), XMLError> {
        self.try_close_start_element()?;
        write_tokens!(self, "<", qname);
        self.name_stack.push(qname.to_owned());
        self.state = State::StartElement;
        Ok(())
    }
    pub fn write_start_element_ns(
        &mut self,
        namespace_name: Option<&str>,
        local_name: &str,
    ) -> Result<(), XMLError> {
        self.try_close_start_element()?;
        if let Some(namespace_name) = namespace_name {
            let Some(pos) = self.ns_stack.iter().rposition(|s| s.0 == namespace_name) else {
                todo!("raise error")
            };
            if !self.ns_stack[pos].1.is_empty() {
                let qname = format!("{}:{}", self.ns_stack[pos].1, local_name);
                self.write_start_element(&qname)
            } else {
                self.write_start_element(local_name)
            }
        } else {
            self.write_start_element(local_name)?;
            if let Some(pos) = self.checker.namespaces.get("")
                && !pos.namespace_name.is_empty()
            {
                self.should_insert_default_namespace = true;
            }
            Ok(())
        }
    }
    pub fn write_end_element(&mut self) -> Result<(), XMLError> {
        self.try_close_start_element()?;
        while self
            .ns_stack
            .pop_if(|l| l.2 == self.name_stack.len())
            .is_some()
        {}
        let qname = self.name_stack.pop().ok_or(XMLError::ParserInvalidEndTag)?;
        write_tokens!(self, "</", &qname, ">");
        Ok(())
    }
    pub fn write_attribute(&mut self, qname: &str, value: &str) -> Result<(), XMLError> {
        let value = value
            .replace('&', "&amp;")
            .replace('"', "&quot;")
            .replace('<', "&lt;");
        write_tokens!(self, " ", qname, "=\"", &value, "\"");
        Ok(())
    }
    pub fn write_attribute_ns(
        &mut self,
        namespace_name: Option<&str>,
        local_name: &str,
        value: &str,
    ) -> Result<(), XMLError> {
        let qname = if let Some(namespace_name) = namespace_name {
            let Some(pos) = self.ns_stack.iter().rposition(|s| s.0 == namespace_name) else {
                todo!("raise error")
            };

            if !self.ns_stack[pos].1.is_empty() {
                format!("{}:{}", self.ns_stack[pos].1, local_name)
            } else {
                local_name.to_owned()
            }
        } else {
            local_name.to_owned()
        };
        self.write_attribute(&qname, value)
    }
    pub fn write_namespace_decl(
        &mut self,
        prefix: Option<&str>,
        namespace_name: &str,
    ) -> Result<(), XMLError> {
        if prefix.is_none() && namespace_name.is_empty() {
            self.should_insert_default_namespace = false;
        }
        let local = prefix.map(|p| format!(":{p}"));
        let local = local.as_deref().unwrap_or_default();
        write_tokens!(self, " xmlns", local, "=\"", namespace_name, "\"");
        self.ns_stack.push((
            namespace_name.to_owned(),
            prefix.unwrap_or_default().to_owned(),
            self.name_stack.len(),
        ));
        Ok(())
    }

    pub fn write_characters(&mut self, data: &str) -> Result<(), XMLError> {
        self.try_close_start_element()?;
        let data = data
            .replace('&', "&amp;")
            .replace("<", "&lt;")
            .replace(">", "&gt;");
        write_tokens!(self, &data);
        Ok(())
    }
    pub fn write_cdata(&mut self, data: &str) -> Result<(), XMLError> {
        self.try_close_start_element()?;
        write_tokens!(self, "<![CDATA[", data, "]]>");
        Ok(())
    }

    pub fn write_comment(&mut self, data: &str) -> Result<(), XMLError> {
        self.try_start_internal_subset()?;
        self.try_close_start_element()?;
        write_tokens!(self, "<!--", data, "-->");
        Ok(())
    }

    pub fn write_processing_instruction(
        &mut self,
        target: &str,
        data: Option<&str>,
    ) -> Result<(), XMLError> {
        self.try_start_internal_subset()?;
        self.try_close_start_element()?;
        let data = data.map(|d| format!(" {d}"));
        let data = data.as_deref().unwrap_or_default();
        write_tokens!(self, "<?", target, data, "?>");
        Ok(())
    }

    pub fn write_general_entity_reference(&mut self, name: &str) -> Result<(), XMLError> {
        self.try_close_start_element()?;
        write_tokens!(self, "&", name, ";");
        Ok(())
    }
    pub fn write_parameter_entity_reference(&mut self, name: &str) -> Result<(), XMLError> {
        self.try_start_internal_subset()?;
        write_tokens!(self, "%", name, ";");
        Ok(())
    }

    fn try_start_internal_subset(&mut self) -> Result<(), XMLError> {
        if matches!(self.state, State::StartDTD) {
            write_tokens!(self, " [");
            self.state = State::InDTD;
        }
        Ok(())
    }
    fn try_close_start_element(&mut self) -> Result<(), XMLError> {
        if matches!(self.state, State::StartElement) {
            if self.should_insert_default_namespace {
                self.write_namespace_decl(None, "")?;
            }
            write_tokens!(self, ">");
            self.state = State::InContent;
        }
        Ok(())
    }

    pub fn flush(&mut self) -> Result<(), XMLError> {
        self.checker.parse_chunk([], true)?;
        self.stream.flush()?;
        self.state = State::Finished;
        Ok(())
    }

    pub fn reset(&mut self) -> Result<(), XMLError> {
        self.stream = OutputStream::default();
        self.checker.reset()?;
        self.name_stack.clear();
        self.ns_stack.clear();
        self.state = State::BeforeStart;
        Ok(())
    }
}

impl<H: SAXHandler> Drop for XMLStreamWriter<'_, H> {
    fn drop(&mut self) {
        self.flush().ok();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn write_declaration_tests() {
        let mut buffer = vec![];
        let mut writer = XMLStreamWriter::new(&mut buffer, None);
        writer.write_declaration("1.0", None, None).unwrap();
        writer.write_start_element("root").unwrap();
        writer.write_end_element().unwrap();
        writer.flush().unwrap();
        drop(writer);
        let ret = String::from_utf8(buffer).unwrap();
        assert_eq!(ret, r#"<?xml version="1.0"?><root></root>"#);
    }

    #[test]
    fn write_declaration_with_encoding_tests() {
        let mut buffer = vec![];
        let mut writer = XMLStreamWriter::new(&mut buffer, None);
        writer
            .write_declaration("1.0", Some("UTF-8"), None)
            .unwrap();
        writer.write_start_element("root").unwrap();
        writer.write_end_element().unwrap();
        writer.flush().unwrap();
        drop(writer);
        let ret = String::from_utf8(buffer).unwrap();
        assert_eq!(
            ret,
            r#"<?xml version="1.0" encoding="UTF-8"?><root></root>"#
        );
    }

    #[test]
    fn write_declaration_with_sa_tests() {
        let mut buffer = vec![];
        let mut writer = XMLStreamWriter::new(&mut buffer, None);
        writer
            .write_declaration("1.0", Some("UTF-8"), Some(true))
            .unwrap();
        writer.write_start_element("root").unwrap();
        writer.write_end_element().unwrap();
        writer.flush().unwrap();
        drop(writer);
        let ret = String::from_utf8(buffer).unwrap();
        assert_eq!(
            ret,
            r#"<?xml version="1.0" encoding="UTF-8" standalone="yes"?><root></root>"#
        );
    }

    #[test]
    fn write_element_withtout_declaration_tests() {
        let mut buffer = vec![];
        let mut writer = XMLStreamWriter::new(&mut buffer, None);
        writer.write_start_element("root").unwrap();
        writer.write_end_element().unwrap();
        writer.flush().unwrap();
        drop(writer);
        let ret = String::from_utf8(buffer).unwrap();
        assert_eq!(ret, r#"<root></root>"#);
    }

    #[test]
    fn write_attributes_tests() {
        let mut buffer = vec![];
        let mut writer = XMLStreamWriter::new(&mut buffer, None);
        writer.write_declaration("1.0", None, None).unwrap();
        writer.write_start_element("root").unwrap();
        writer.write_attribute("att", "att").unwrap();
        writer.write_attribute("att2", "att2").unwrap();
        writer.write_end_element().unwrap();
        writer.flush().unwrap();
        drop(writer);
        let ret = String::from_utf8(buffer).unwrap();
        assert_eq!(
            ret,
            r#"<?xml version="1.0"?><root att="att" att2="att2"></root>"#
        );
    }

    #[test]
    fn write_escaped_attribute_value_tests() {
        let mut buffer = vec![];
        let mut writer = XMLStreamWriter::new(&mut buffer, None);
        writer.write_start_element("root").unwrap();
        writer.write_attribute("att", r#""'&<>"#).unwrap();
        writer.write_end_element().unwrap();
        writer.flush().unwrap();
        drop(writer);
        let ret = String::from_utf8(buffer).unwrap();
        assert_eq!(ret, r#"<root att="&quot;'&amp;&lt;>"></root>"#);
    }

    #[test]
    fn write_nested_content_tests() {
        let mut buffer = vec![];
        let mut writer = XMLStreamWriter::new(&mut buffer, None);
        writer.write_start_element("root").unwrap();
        writer.write_characters("first").unwrap();
        writer.write_start_element("child").unwrap();
        writer.write_characters("second").unwrap();
        writer.write_characters(" third").unwrap();
        writer.write_end_element().unwrap();
        writer.write_characters("forth").unwrap();
        writer.write_end_element().unwrap();
        writer.flush().unwrap();
        drop(writer);
        let ret = String::from_utf8(buffer).unwrap();
        assert_eq!(ret, r#"<root>first<child>second third</child>forth</root>"#);
    }

    #[test]
    fn write_escaped_content_tests() {
        let mut buffer = vec![];
        let mut writer = XMLStreamWriter::new(&mut buffer, None);
        writer.write_start_element("root").unwrap();
        writer.write_start_element("child1").unwrap();
        writer.write_characters(r#""'&<>"#).unwrap();
        writer.write_end_element().unwrap();
        writer.write_start_element("child2").unwrap();
        writer.write_characters("]]>").unwrap();
        writer.write_end_element().unwrap();
        writer.write_end_element().unwrap();
        writer.flush().unwrap();
        drop(writer);
        let ret = String::from_utf8(buffer).unwrap();
        assert_eq!(
            ret,
            r#"<root><child1>"'&amp;&lt;&gt;</child1><child2>]]&gt;</child2></root>"#
        );
    }

    #[test]
    fn write_cdata_section_tests() {
        let mut buffer = vec![];
        let mut writer = XMLStreamWriter::new(&mut buffer, None);
        writer.write_start_element("root").unwrap();
        writer.write_cdata(r#"normal text"#).unwrap();
        writer.write_cdata(r#""'&<>"#).unwrap();
        writer.write_end_element().unwrap();
        writer.flush().unwrap();
        drop(writer);
        let ret = String::from_utf8(buffer).unwrap();
        assert_eq!(
            ret,
            r#"<root><![CDATA[normal text]]><![CDATA["'&<>]]></root>"#
        );
    }

    #[test]
    fn write_nsdecl_tests() {
        let mut buffer = vec![];
        let mut writer = XMLStreamWriter::new(&mut buffer, None);
        writer.write_start_element("ex:root").unwrap();
        writer
            .write_namespace_decl(Some("ex"), "http://example.com/1")
            .unwrap();
        writer
            .write_start_element_ns(Some("http://example.com/1"), "child")
            .unwrap();
        writer.write_end_element().unwrap();
        writer.write_end_element().unwrap();
        writer.flush().unwrap();
        drop(writer);
        let ret = String::from_utf8(buffer).unwrap();
        assert_eq!(
            ret,
            r#"<ex:root xmlns:ex="http://example.com/1"><ex:child></ex:child></ex:root>"#
        );
    }

    #[test]
    fn write_default_ns_auto_insertion_tests() {
        let mut buffer = vec![];
        let mut writer = XMLStreamWriter::new(&mut buffer, None);
        writer.write_start_element("root").unwrap();
        writer
            .write_namespace_decl(None, "http://example.com/1")
            .unwrap();
        writer.write_start_element_ns(None, "child").unwrap();
        writer.write_end_element().unwrap();
        writer.write_end_element().unwrap();
        writer.flush().unwrap();
        drop(writer);
        let ret = String::from_utf8(buffer).unwrap();
        assert_eq!(
            ret,
            r#"<root xmlns="http://example.com/1"><child xmlns=""></child></root>"#
        );
    }

    #[test]
    fn write_dtd_tests() {
        let mut buffer = vec![];
        let mut writer = XMLStreamWriter::new(&mut buffer, None);
        writer
            .write_start_dtd("root", None, Some("sample.dtd"))
            .unwrap();
        writer
            .write_element_decl("root", "(#PCDATA|child)*")
            .unwrap();
        writer.write_element_decl("child", "EMPTY").unwrap();
        writer
            .write_attribute_decl("root", "att", "NMTOKEN", "#IMPLIED")
            .unwrap();
        writer
            .write_internal_entity_decl("ent", "entity content")
            .unwrap();
        writer.write_end_dtd().unwrap();
        writer.write_start_element("root").unwrap();
        writer.write_attribute("att", "token").unwrap();
        writer.write_start_element("child").unwrap();
        writer.write_end_element().unwrap();
        writer.write_general_entity_reference("ent").unwrap();
        writer.write_end_element().unwrap();
        writer.flush().unwrap();
        drop(writer);
        let ret = String::from_utf8(buffer).unwrap();
        assert_eq!(
            ret,
            r#"<!DOCTYPE root SYSTEM "sample.dtd" [<!ELEMENT root (#PCDATA|child)*><!ELEMENT child EMPTY><!ATTLIST root att NMTOKEN #IMPLIED><!ENTITY ent "entity content">]><root att="token"><child></child>&ent;</root>"#
        );
    }

    #[test]
    fn write_misc_tests() {
        let mut buffer = vec![];
        let mut writer = XMLStreamWriter::new(&mut buffer, None);
        writer.write_comment("comment1").unwrap();
        writer
            .write_processing_instruction("pi1", Some("data1"))
            .unwrap();
        writer.write_start_element("root").unwrap();
        writer.write_comment("comment2").unwrap();
        writer.write_processing_instruction("pi2", None).unwrap();
        writer.write_end_element().unwrap();
        writer.write_comment("comment3").unwrap();
        writer.write_processing_instruction("pi3", None).unwrap();
        writer.flush().unwrap();
        drop(writer);
        let ret = String::from_utf8(buffer).unwrap();
        assert_eq!(
            ret,
            r#"<!--comment1--><?pi1 data1?><root><!--comment2--><?pi2?></root><!--comment3--><?pi3?>"#
        );
    }
}
