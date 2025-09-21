use crate::{XMLVersion, sax::attributes::Attributes};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub(crate) enum XMLEventType {
    #[default]
    StartDocument,
    EndDocument,
    StartElement,
    EndElement,
    Declaration,
    DocumentType,
    Characters,
    CDATASection,
    Space,
    Comment,
    ProcessingInstruction,
    EntityReference,
    Finished,
}

pub enum XMLEvent<'a> {
    StartDocument,
    EndDocument,
    StartElement(StartElement<'a>),
    EndElement(EndElement<'a>),
    Declaration(Declaration<'a>),
    DocumentType,
    Characters(&'a str),
    CDATASection(&'a str),
    Space(&'a str),
    Comment(&'a str),
    ProcessingInstruction(ProcessingInstruction<'a>),
    EntityReference(&'a str),
    Finished,
}

pub struct StartElement<'a> {
    pub(super) namespace_name: Option<&'a str>,
    pub(super) local_name: Option<&'a str>,
    pub(super) qname: &'a str,
    pub(super) atts: &'a Attributes,
}

impl StartElement<'_> {
    pub fn name(&self) -> &str {
        self.qname
    }

    pub fn local_name(&self) -> Option<&str> {
        self.local_name
    }

    pub fn prefix(&self) -> Option<&str> {
        let local_name = self.local_name?;
        let prefix_len = self.qname.len() - local_name.len();
        (prefix_len > 0).then_some(&self.qname[..prefix_len - 1])
    }

    pub fn namespace_uri(&self) -> Option<&str> {
        self.namespace_name
    }

    pub fn attributes(&self) -> &Attributes {
        self.atts
    }
}

pub struct EndElement<'a> {
    pub(super) namespace_name: Option<&'a str>,
    pub(super) local_name: Option<&'a str>,
    pub(super) qname: &'a str,
}

impl EndElement<'_> {
    pub fn name(&self) -> &str {
        self.qname
    }

    pub fn local_name(&self) -> Option<&str> {
        self.local_name
    }

    pub fn prefix(&self) -> Option<&str> {
        let local_name = self.local_name?;
        let prefix_len = self.qname.len() - local_name.len();
        (prefix_len > 0).then_some(&self.qname[..prefix_len - 1])
    }

    pub fn namespace_uri(&self) -> Option<&str> {
        self.namespace_name
    }
}

pub struct Declaration<'a> {
    pub(super) version: XMLVersion,
    pub(super) encoding: Option<&'a str>,
    pub(super) standalone: Option<bool>,
}

impl Declaration<'_> {
    pub fn version(&self) -> XMLVersion {
        self.version
    }

    pub fn encoding(&self) -> Option<&str> {
        self.encoding
    }

    /// If explicitly set to `standalone='yes'`, returns `true`.  \
    /// If `standalone='no'` or no standalone declaration exists, returns `false`.
    pub fn is_standalone(&self) -> bool {
        self.standalone == Some(true)
    }

    /// If the standalone declaration exists, return a `Some` wrapping a Boolean value corresponding
    /// to that declaration.  \
    /// If no standalone declaration exists, return `None`.
    pub fn standalone(&self) -> Option<bool> {
        self.standalone
    }
}

pub struct ProcessingInstruction<'a> {
    pub(super) target: &'a str,
    pub(super) data: Option<&'a str>,
}

impl ProcessingInstruction<'_> {
    pub fn target(&self) -> &str {
        self.target
    }

    pub fn data(&self) -> Option<&str> {
        self.data
    }
}
