//! Provide StAX parser events.

use crate::{XMLVersion, sax::Attributes};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub(crate) enum XMLEventType {
    #[default]
    StartDocument,
    EndDocument,
    StartElement,
    EndElement,
    StartEmptyTag,
    EndEmptyTag,
    Declaration,
    DocumentType,
    Characters,
    CDATASection,
    Space,
    Comment,
    ProcessingInstruction,
    StartEntity,
    EndEntity,
    FatalError,
    Finished,
}

/// Parser event from StAX parser.
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
    StartEntity(&'a str),
    EndEntity,
    FatalError,
    Finished,
}

impl std::fmt::Debug for XMLEvent<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            XMLEvent::StartDocument => write!(f, "startDocument()"),
            XMLEvent::EndDocument => write!(f, "endDocument()"),
            XMLEvent::StartElement(start) => {
                write!(
                    f,
                    "startElement({}, {}, {}, {}",
                    start.namespace_name().unwrap_or("None"),
                    start.prefix().unwrap_or("None"),
                    start.local_name().unwrap_or("None"),
                    start.name()
                )?;

                for att in start.attributes() {
                    write!(f, ", ")?;
                    if let Some(local_name) = att.local_name.as_deref() {
                        if let Some(uri) = att.namespace_name.as_deref() {
                            write!(f, "{{{uri}}}")?;
                        }
                        write!(f, "{local_name}='{}'", att.value)?;
                    } else {
                        write!(f, "{}='{}'", att.qname, att.value)?;
                    }
                }
                write!(f, ")")
            }
            XMLEvent::EndElement(end) => {
                write!(
                    f,
                    "endElement({}, {}, {}, {})",
                    end.namespace_name().unwrap_or("None"),
                    end.prefix().unwrap_or("None"),
                    end.local_name().unwrap_or("None"),
                    end.name()
                )
            }
            XMLEvent::Declaration(declaration) => {
                write!(
                    f,
                    "declaration({}, {}, ",
                    declaration.version(),
                    declaration.encoding().unwrap_or("None")
                )?;
                if let Some(standalone) = declaration.standalone() {
                    if standalone {
                        write!(f, "yes)")
                    } else {
                        write!(f, "no)")
                    }
                } else {
                    write!(f, "None)")
                }
            }
            XMLEvent::DocumentType => write!(f, "documentType()"),
            XMLEvent::Characters(characters) => {
                write!(f, "characters({characters})")
            }
            XMLEvent::CDATASection(cdata) => write!(f, "cdataSection({cdata})"),
            XMLEvent::Space(space) => write!(f, "space({space})"),
            XMLEvent::Comment(comment) => write!(f, "comment({comment})"),
            XMLEvent::ProcessingInstruction(pi) => write!(
                f,
                "processingInstruction({}, '{}')",
                pi.target(),
                pi.data().unwrap_or("None")
            ),
            XMLEvent::StartEntity(entity) => write!(f, "startEntity({entity})"),
            XMLEvent::EndEntity => write!(f, "endEntity()"),
            XMLEvent::FatalError => write!(f, "fatalError()"),
            XMLEvent::Finished => write!(f, "finished()"),
        }
    }
}

/// Start element event.
pub struct StartElement<'a> {
    pub(super) namespace_name: Option<&'a str>,
    pub(super) local_name: Option<&'a str>,
    pub(super) qname: &'a str,
    pub(super) atts: &'a Attributes,
}

impl StartElement<'_> {
    /// QName of this element.
    pub fn name(&self) -> &str {
        self.qname
    }

    /// Local part of QName of this element.
    pub fn local_name(&self) -> Option<&str> {
        self.local_name
    }

    /// Prefix of QName of this element.
    pub fn prefix(&self) -> Option<&str> {
        let local_name = self.local_name?;
        let prefix_len = self.qname.len() - local_name.len();
        (prefix_len > 0).then(|| &self.qname[..prefix_len - 1])
    }

    /// Namespace name of this element.
    pub fn namespace_name(&self) -> Option<&str> {
        self.namespace_name
    }

    /// Attributes of this element.
    pub fn attributes(&self) -> &Attributes {
        self.atts
    }
}

/// End element event.
pub struct EndElement<'a> {
    pub(super) namespace_name: Option<&'a str>,
    pub(super) local_name: Option<&'a str>,
    pub(super) qname: &'a str,
}

impl EndElement<'_> {
    /// QName of this element.
    pub fn name(&self) -> &str {
        self.qname
    }

    /// Local part of QName of this element.
    pub fn local_name(&self) -> Option<&str> {
        self.local_name
    }

    /// Prefix of QName of this element.
    pub fn prefix(&self) -> Option<&str> {
        let local_name = self.local_name?;
        let prefix_len = self.qname.len() - local_name.len();
        (prefix_len > 0).then(|| &self.qname[..prefix_len - 1])
    }

    /// Namespace name of this element.
    pub fn namespace_name(&self) -> Option<&str> {
        self.namespace_name
    }
}

/// Declaration event.
pub struct Declaration<'a> {
    pub(super) version: XMLVersion,
    pub(super) encoding: Option<&'a str>,
    pub(super) standalone: Option<bool>,
}

impl Declaration<'_> {
    /// XML version.
    pub fn version(&self) -> XMLVersion {
        self.version
    }

    /// Encoding name specified at this declaration.
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

/// Processing instruction event.
pub struct ProcessingInstruction<'a> {
    pub(super) target: &'a str,
    pub(super) data: Option<&'a str>,
}

impl ProcessingInstruction<'_> {
    /// Target name of this processing instruction.
    pub fn target(&self) -> &str {
        self.target
    }

    /// Data of this processing instruction.
    pub fn data(&self) -> Option<&str> {
        self.data
    }
}
